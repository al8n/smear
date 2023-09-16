use ::uuid::Uuid;

impl_diagnostic_and_encodable!(string(Uuid::parse_uuid));
