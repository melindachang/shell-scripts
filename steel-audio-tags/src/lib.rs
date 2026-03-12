#![warn(clippy::pedantic)]

use abi_stable::std_types::{RBoxError, RResult};
use anyhow::{anyhow, Context};
use lofty::{
    config::{ParseOptions, WriteOptions},
    file::{BoundTaggedFile, TaggedFileExt},
    tag::{ItemValue, Tag, TagItem},
};
use regex::Regex;
use std::{
    collections::HashMap,
    fs::{File, OpenOptions},
    path::Path,
};
use steel::{
    declare_module,
    steel_vm::ffi::{FFIModule, FFIValue, IntoFFIVal, RegisterFFIFn},
};

#[derive(Debug)]
enum TagValue {
    String(String),
    VecString(Vec<String>),
}

impl IntoFFIVal for TagValue {
    fn into_ffi_val(self) -> RResult<FFIValue, RBoxError> {
        match self {
            TagValue::String(s) => RResult::ROk(s.into()),
            TagValue::VecString(v) => RResult::ROk(v.into_ffi_val().unwrap()),
        }
    }
}

struct FFIError(anyhow::Error);

impl core::fmt::Debug for FFIError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl IntoFFIVal for FFIError {
    fn into_ffi_val(self) -> RResult<FFIValue, RBoxError> {
        let error: Box<dyn std::error::Error + Send + Sync> = format!("{:?}", self.0).into();
        RResult::RErr(RBoxError::from_box(error))
    }
}

fn with_bound_tagged_file<F, R>(path_str: &str, f: F) -> anyhow::Result<R>
where
    F: FnOnce(&mut BoundTaggedFile<File>) -> anyhow::Result<R>,
{
    let path = Path::new(&path_str);
    let file = OpenOptions::new().read(true).write(true).open(path)?;

    let mut bound_tagged_file = BoundTaggedFile::read_from(file, ParseOptions::new())?;

    f(&mut bound_tagged_file)
}

/// extract-audio-tags : string? -> (hashof string? (or/c string? (listof string?)))
fn extract_audio_tags(path_str: &str) -> Result<HashMap<String, TagValue>, FFIError> {
    with_bound_tagged_file(path_str, |tagged_file| {
        let tag = tagged_file
            .primary_tag()
            .or_else(|| tagged_file.first_tag())
            .context("No tags found")?;
        Ok(tag_to_map(tag))
    })
    .map_err(FFIError)
}

fn tag_to_map(tag: &Tag) -> HashMap<String, TagValue> {
    let mut tag_map: HashMap<String, TagValue> = HashMap::new();

    for item in tag.items() {
        let key_str = format!("{:?}", item.key()).to_lowercase();
        let value_str = item.value().text().unwrap_or("");

        tag_map
            .entry(key_str)
            .and_modify(|existing| match existing {
                TagValue::VecString(v) => {
                    v.push(value_str.to_string());
                }
                TagValue::String(s) => {
                    if s != value_str {
                        let old_str = std::mem::take(s);
                        *existing = TagValue::VecString(vec![old_str, value_str.to_string()]);
                    }
                }
            })
            .or_insert(TagValue::String(value_str.to_string()));
    }

    tag_map
}

fn regex_patch_audio_tag(
    path_str: &str,
    keys_to_patch: Vec<String>,
    raw_rules: Vec<Vec<String>>, // (pattern . replacement)
) -> anyhow::Result<(), FFIError> {
    with_bound_tagged_file(path_str, |tagged_file| {
        let rules: Vec<(Regex, &String)> = match raw_rules
            .iter()
            .map(|r| Regex::new(&r[0]).map(|re| (re, &r[1])))
            .collect::<Result<Vec<_>, _>>()
        {
            Ok(r) => r,
            Err(e) => return Err(anyhow!("Invalid regex: {e}")),
        };

        let tag = if let Some(t) = tagged_file.primary_tag_mut() {
            t
        } else if let Some(t) = tagged_file.first_tag_mut() {
            t
        } else {
            return Err(anyhow!("No tags found"));
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
    })
    .map_err(FFIError)
}

declare_module!(create_module);

fn create_module() -> FFIModule {
    let mut module = FFIModule::new("steel/audio-tags");
    module
        .register_fn("extract-audio-tags", extract_audio_tags)
        .register_fn("regex-patch-audio-tag", regex_patch_audio_tag);

    module
}
