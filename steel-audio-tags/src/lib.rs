use abi_stable::std_types::{RResult::ROk, RString};
use anyhow::{Context, Result};
use lofty::{file::TaggedFileExt, probe::Probe, tag::Tag};
use std::{collections::HashMap, path::Path};
use steel::{
    declare_module,
    steel_vm::ffi::{FFIModule, FFIValue, IntoFFIVal, RegisterFFIFn},
};

fn extract_audio_tags(path_str: String) -> FFIValue {
    let result = || -> Result<FFIValue> {
        let path = Path::new(&path_str);
        let tagged_file = Probe::open(path)?.read()?;

        let tag = match tagged_file.primary_tag() {
            Some(primary_tag) => primary_tag,
            None => tagged_file.first_tag().context("No tags found")?,
        };

        tag_to_steel_map(tag)
    };

    result().unwrap_or(FFIValue::BoolV(false))
}

#[derive(Debug)]
enum TagValue {
    String(String),
    VecString(Vec<String>),
}

impl IntoFFIVal for TagValue {
    fn into_ffi_val(
        self,
    ) -> abi_stable::std_types::RResult<FFIValue, abi_stable::std_types::RBoxError> {
        match self {
            TagValue::String(s) => ROk(FFIValue::StringV(RString::from(s))),
            TagValue::VecString(v) => ROk(FFIValue::Vector(
                v.into_iter()
                    .map(|s| FFIValue::StringV(RString::from(s)))
                    .collect(),
            )),
        }
    }
}

fn tag_to_steel_map(tag: &Tag) -> Result<FFIValue> {
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

    Ok(tag_map.into_ffi_val().unwrap())
}

declare_module!(create_module);

fn create_module() -> FFIModule {
    let mut module = FFIModule::new("steel/audio-tags");
    module.register_fn("extract-audio-tags", extract_audio_tags);
    module
}
