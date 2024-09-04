use std::iter::once;

use heck::{ToLowerCamelCase, ToUpperCamelCase};

const KEYWORDS: &[&'static str] = &["default", "switch", "package", "private", "readonly"];

const TYPE_KEYWORDS: &[&'static str] = &["Parameters"];

pub(crate) fn normalize_name(name: &str) -> String {
    if KEYWORDS.iter().any(|x| *x == name) {
        format!("{}_", name.to_lower_camel_case())
    } else {
        name.to_lower_camel_case()
    }
}

pub(crate) fn normalize_type_name(type_name: &str) -> String {
    if TYPE_KEYWORDS.iter().any(|x| *x == type_name) {
        format!("{}_", type_name.to_upper_camel_case())
    } else {
        type_name.to_upper_camel_case()
    }
}

pub(crate) fn ns_name(ns: &[String], name: &str) -> String {
    format!(
        ".{}",
        ns.iter()
            .cloned()
            .chain(once(name.to_string()))
            .collect::<Vec<_>>()
            .join(".")
    )
}

