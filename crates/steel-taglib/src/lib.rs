#![warn(clippy::pedantic)]

use abi_stable::std_types::{RBoxError, RResult};
use anyhow::{anyhow, Context};
use lofty::{
    config::{ParseOptions, WriteOptions},
    file::{AudioFile, BoundTaggedFile, TaggedFileExt},
    read_from_path,
    tag::{ItemValue, Tag, TagItem},
};
use regex::Regex;
use std::{
    collections::HashMap,
    fs::{self, OpenOptions},
    io,
};
use steel::{
    declare_module,
    steel_vm::ffi::{FFIModule, FFIValue, IntoFFIVal, RegisterFFIFn},
};

enum Either<L, R> {
    Left(L),
    Right(R),
}

impl<L: IntoFFIVal, R: IntoFFIVal> IntoFFIVal for Either<L, R> {
    fn into_ffi_val(self) -> RResult<FFIValue, RBoxError> {
        match self {
            Self::Left(l) => l.into_ffi_val(),
            Self::Right(r) => r.into_ffi_val(),
        }
    }
}

struct FFIError(anyhow::Error);

impl core::fmt::Debug for FFIError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl<E: Into<anyhow::Error>> From<E> for FFIError {
    fn from(err: E) -> Self {
        FFIError(err.into())
    }
}

impl IntoFFIVal for FFIError {
    fn into_ffi_val(self) -> RResult<FFIValue, RBoxError> {
        let error: Box<dyn std::error::Error + Send + Sync> = format!("{:?}", self.0).into();
        RResult::RErr(RBoxError::from_box(error))
    }
}

/// get-audio-tags : string? -> (hashof string? (or/c string? (listof string?)))
fn get_audio_tags(
    path_str: &str,
) -> Result<HashMap<String, Either<String, Vec<String>>>, FFIError> {
    let tagged_file = read_from_path(path_str)?;
    let tag = tagged_file
        .primary_tag()
        .or_else(|| tagged_file.first_tag())
        .with_context(|| format!("No tags found: {path_str}"))?;

    Ok(tag_to_map(tag))
}

/// get-audio-properties : string? -> (hashof string? (or/c number? #f))
fn get_audio_properties(path_str: &str) -> Result<HashMap<String, Option<usize>>, FFIError> {
    let audio_file = read_from_path(path_str)?;
    let props = audio_file.properties();

    let mut prop_map: HashMap<String, Option<usize>> = HashMap::new();

    prop_map.insert(
        "duration".into(),
        Some(props.duration().as_millis() as usize),
    );
    prop_map.insert(
        "overall-bitrate".into(),
        props.overall_bitrate().map(|v| v as usize),
    );
    prop_map.insert(
        "audio-bitrate".into(),
        props.audio_bitrate().map(|v| v as usize),
    );
    prop_map.insert("bit-depth".into(), props.bit_depth().map(|v| v as usize));

    Ok(prop_map)
}

fn tag_to_map(tag: &Tag) -> HashMap<String, Either<String, Vec<String>>> {
    let mut tag_map: HashMap<String, Either<String, Vec<String>>> = HashMap::new();

    for item in tag.items() {
        let key_str = format!("{:?}", item.key()).to_lowercase();
        let value_str = item.value().text().unwrap_or("");

        tag_map
            .entry(key_str)
            .and_modify(|existing| match existing {
                Either::Left(s) => {
                    if s != value_str {
                        let old_str = std::mem::take(s);
                        *existing = Either::Right(vec![old_str, value_str.to_string()]);
                    }
                }
                Either::Right(v) => v.push(value_str.to_string()),
            })
            .or_insert(Either::Left(value_str.to_string()));
    }

    tag_map
}

/// regex-patch-audio-tags : string? (vectorof string?) (vectorof (vectorof string?)) -> void?
fn regex_patch_audio_tag(
    path_str: &str,
    keys_to_patch: Vec<String>,
    raw_rules: Vec<Vec<String>>, // (pattern . replacement)
) -> Result<(), FFIError> {
    let rules: Vec<(Regex, &String)> = raw_rules
        .iter()
        .map(|r| Regex::new(&r[0]).map(|re| (re, &r[1])))
        .collect::<Result<Vec<_>, _>>()?;

    let file = OpenOptions::new().read(true).write(true).open(path_str)?;

    let mut tagged_file = BoundTaggedFile::read_from(file, ParseOptions::new())?;

    let tag = if tagged_file.primary_tag().is_some() {
        tagged_file.primary_tag_mut().unwrap()
    } else {
        tagged_file
            .first_tag_mut()
            .ok_or_else(|| anyhow!("No tags found in file"))?
    };

    let mut item_keys = Vec::new();
    for item in tag.items() {
        let current_key_str = format!("{:?}", item.key()).to_lowercase();
        if keys_to_patch.contains(&current_key_str) {
            item_keys.push(item.key());
        }
    }

    let mut is_dirty = false;

    for key in item_keys {
        let items: Vec<TagItem> = tag.take(key).collect();
        for item in items {
            let val_str = item
                .into_value()
                .into_string()
                .context("Non-text tag encountered")?;

            let mut cleaned = val_str.clone();

            for (re, replacement) in &rules {
                cleaned = re.replace_all(&cleaned, *replacement).to_string();
            }

            if cleaned != val_str {
                if !is_dirty {
                    println!("{path_str}");
                    is_dirty = true;
                }
                println!("└─ {key:?} :: {val_str} -> {cleaned}");
            }

            let new_item = TagItem::new_checked(tag.tag_type(), key, ItemValue::Text(cleaned))
                .context("Invalid key for tag type")?;

            tag.push(new_item);
        }
    }

    if is_dirty {
        tagged_file.save(WriteOptions::default())?;
        println!();
    }

    Ok(())
}

fn remove_all_images(path_str: &str) -> Result<(), FFIError> {
    let file = OpenOptions::new().read(true).write(true).open(path_str)?;

    let mut tagged_file = BoundTaggedFile::read_from(file, ParseOptions::new())?;

    let tag = if tagged_file.primary_tag().is_some() {
        tagged_file.primary_tag_mut().unwrap()
    } else {
        tagged_file
            .first_tag_mut()
            .ok_or_else(|| anyhow!("No tags found in file"))?
    };

    let n_images = tag.picture_count();

    if n_images > 0 {
        for i in 0..n_images {
            tag.remove_picture(i as usize);
        }

        println!("Removed {n_images} image(s) from {path_str}");

        tagged_file.save(WriteOptions::default())?;
    }

    Ok(())
}

/// rename-file! : string? string? -> void?
fn rename_file(source: &str, destination: &str) -> Result<(), FFIError> {
    if let Err(e) = fs::rename(source, destination) {
        if e.raw_os_error() == Some(18) || e.kind() == io::ErrorKind::CrossesDevices {
            fs::copy(source, destination)?;
            fs::remove_file(source)?;
        } else {
            return Err(e.into());
        }
    }

    Ok(())
}

declare_module!(create_module);

fn create_module() -> FFIModule {
    let mut module = FFIModule::new("steel/taglib");
    module
        .register_fn("get-audio-tags", get_audio_tags)
        .register_fn("regex-patch-audio-tag", regex_patch_audio_tag)
        .register_fn("get-audio-properties", get_audio_properties)
        .register_fn("remove-all-images!", remove_all_images)
        .register_fn("rename-file!", rename_file);

    module
}
