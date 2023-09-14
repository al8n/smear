#[cfg(feature = "chrono")]
#[cfg_attr(docsrs, doc(cfg(feature = "chrono")))]
mod chrono;

#[cfg(feature = "decimal")]
#[cfg_attr(docsrs, doc(cfg(feature = "decimal")))]
mod decimal;

#[cfg(feature = "bigint")]
#[cfg_attr(docsrs, doc(cfg(feature = "bigint")))]
mod bigint;

#[cfg(feature = "bigdecimal")]
#[cfg_attr(docsrs, doc(cfg(feature = "bigdecimal")))]
mod bigdecimal;

#[cfg(feature = "humantime")]
#[cfg_attr(docsrs, doc(cfg(feature = "humantime")))]
mod humantime;

#[cfg(feature = "url")]
#[cfg_attr(docsrs, doc(cfg(feature = "url")))]
mod url;

#[cfg(feature = "uuid")]
#[cfg_attr(docsrs, doc(cfg(feature = "uuid")))]
mod uuid;

#[cfg(feature = "indexmap")]
#[cfg_attr(docsrs, doc(cfg(feature = "indexmap")))]
mod indexmap;
