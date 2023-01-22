const std = @import("std");

pub const generic = @import("generic.zig");
pub const Any = generic.Any;

pub const enums = @import("enums.zig");

test {
    std.testing.refAllDeclsRecursive(enums);
}
