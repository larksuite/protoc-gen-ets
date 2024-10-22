use crate::descriptor::field_descriptor_proto;

/// Tag occupies three bits.
pub(crate) const TAG_TYPE_BITS: u32 = 3;
/// Apply this mask to varint value to obtain a tag.
pub(crate) const TAG_TYPE_MASK: u32 = (1u32 << TAG_TYPE_BITS as usize) - 1;
/// Max possible field number
pub(crate) const FIELD_NUMBER_MAX: u32 = 0x1fffffff;


/// Enum values added here for diagnostic purposes.
/// Users should not depend on specific values.
#[derive(Debug, thiserror::Error)]
pub(crate) enum WireError {
    #[error("Unexpected EOF")]
    UnexpectedEof,
    #[error("Unexpected wire type")]
    UnexpectedWireType(WireType),
    #[error("Incorrect tag")]
    IncorrectTag(u32),
    #[error("Incorrect varint")]
    IncorrectVarint,
    #[error("Invalid UTF-8 sequence")]
    Utf8Error,
    #[error("Invalid enum `{}` value: {}", .0, .1)]
    InvalidEnumValue(&'static str, i32),
    #[error("Over recursion limit")]
    OverRecursionLimit,
    #[error("Truncated message")]
    TruncatedMessage,
    // not really possible
    #[error("Limit overflow")]
    LimitOverflow,
    #[error("New limit must not be greater than current limit")]
    LimitIncrease,
    #[error("Encoded message size {0} is too large")]
    MessageTooLarge(u64),
    #[error("Value too large for u32: {}", .0)]
    U32Overflow(u64),
    #[error("Value too large for i32: {}", .0)]
    I32Overflow(i64),
}

/// All supported "wire types" are listed in this enum.
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum WireType {
    /// Variable-length integer
    Varint = 0,
    /// 32-bit field (e. g. `fixed64` or `double`)
    Fixed64 = 1,
    /// Length-delimited field
    LengthDelimited = 2,
    /// Groups are not supported in rust-protobuf
    StartGroup = 3,
    /// Groups are not supported in rust-protobuf
    EndGroup = 4,
    /// 32-bit field (e. g. `fixed32` or `float`)
    Fixed32 = 5,
}

impl WireType {
    /// Construct `WireType` from number, or return `None` if type is unknown.
    pub fn new(n: u32) -> Option<WireType> {
        match n {
            0 => Some(WireType::Varint),
            1 => Some(WireType::Fixed64),
            2 => Some(WireType::LengthDelimited),
            3 => Some(WireType::StartGroup),
            4 => Some(WireType::EndGroup),
            5 => Some(WireType::Fixed32),
            _ => None,
        }
    }

    pub fn for_type(field_type: field_descriptor_proto::Type) -> WireType {
        use field_descriptor_proto::Type;
        match field_type {
            Type::TYPE_INT32 => WireType::Varint,
            Type::TYPE_INT64 => WireType::Varint,
            Type::TYPE_UINT32 => WireType::Varint,
            Type::TYPE_UINT64 => WireType::Varint,
            Type::TYPE_SINT32 => WireType::Varint,
            Type::TYPE_SINT64 => WireType::Varint,
            Type::TYPE_BOOL => WireType::Varint,
            Type::TYPE_ENUM => WireType::Varint,
            Type::TYPE_FIXED32 => WireType::Fixed32,
            Type::TYPE_FIXED64 => WireType::Fixed64,
            Type::TYPE_SFIXED32 => WireType::Fixed32,
            Type::TYPE_SFIXED64 => WireType::Fixed64,
            Type::TYPE_FLOAT => WireType::Fixed32,
            Type::TYPE_DOUBLE => WireType::Fixed64,
            Type::TYPE_STRING => WireType::LengthDelimited,
            Type::TYPE_BYTES => WireType::LengthDelimited,
            Type::TYPE_MESSAGE => WireType::LengthDelimited,
            Type::TYPE_GROUP => WireType::LengthDelimited, // not true
        }
    }
}

/// Parsed field tag (a pair of field number and wire type)
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Tag {
    field_number: u32,
    wire_type: WireType,
}

impl Tag {
    /// Fold tag to a number to be serialized.
    pub fn value(self) -> u32 {
        (self.field_number << TAG_TYPE_BITS) | (self.wire_type as u32)
    }

    /// Extract wire type and field number from integer tag
    pub fn new(value: u32) -> Result<Tag, WireError> {
        let wire_type = WireType::new(value & TAG_TYPE_MASK);
        if wire_type.is_none() {
            return Err(WireError::IncorrectTag(value).into());
        }
        let field_number = value >> TAG_TYPE_BITS;
        if field_number == 0 {
            return Err(WireError::IncorrectTag(value).into());
        }
        Ok(Tag {
            field_number,
            wire_type: wire_type.unwrap(),
        })
    }

    /// Construct a tag from a field number and wire type.
    ///
    /// # Panics
    ///
    /// If field number is outside of valid range.
    pub fn make(field_number: u32, wire_type: WireType) -> Tag {
        assert!(field_number > 0 && field_number <= FIELD_NUMBER_MAX);
        Tag {
            field_number,
            wire_type,
        }
    }

    /// Get field number and wire type
    pub fn unpack(self) -> (u32, WireType) {
        (self.field_number(), self.wire_type())
    }

    /// Get wire type
    fn wire_type(self) -> WireType {
        self.wire_type
    }

    /// Get field number
    pub fn field_number(self) -> u32 {
        self.field_number
    }
}