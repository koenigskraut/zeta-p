const std = @import("std");
const trait = std.meta.trait;
const testing = std.testing;

/// A bit more reliable than passing `*anyopaque`, **only for comptime**.
/// Use `const bar = Any.wrap(&foo);` to wrap pointer to `foo` in `Any` type.
/// Than use `const ptr = bar.unwrap();` to get this pointer back with its preserved type.
/// You can get the type of the content wrapped in `Any` by accessing its `T` field.
pub const Any = struct {
    T: type,
    ptr: union(enum) { Var: *anyopaque, Const: *const anyopaque },

    pub fn wrap(comptime value: anytype) Any {
        const info = @typeInfo(@TypeOf(value));
        if (info != .Pointer) @compileError("Expected a pointer to a const/comptime variable");

        return .{
            .T = @TypeOf(value.*),
            .ptr = if (info.Pointer.is_const) .{ .Const = value } else .{ .Var = value },
        };
    }

    pub fn unwrap(comptime self: Any) if (self.ptr == .Var) *self.T else *const self.T {
        return switch (self.ptr) {
            .Var => |v| @ptrCast(*self.T, @alignCast(@alignOf(*self.T), v)),
            .Const => |c| @ptrCast(*const self.T, @alignCast(@alignOf(*self.T), c)),
        };
    }
};

fn plusOne(comptime a: Any) a.T {
    return a.unwrap().* + 1;
}

test "Any var" {
    comptime var foo = 15;
    const wrapped = Any.wrap(&foo);
    comptime wrapped.unwrap().* += 27;

    try testing.expect(wrapped.T == comptime_int);
    try testing.expect(foo == 42);
}

test "Any const" {
    const foo: usize = 12;
    const bar: f32 = 10.5;
    const wrappedFoo = Any.wrap(&foo);
    const wrappedBar = Any.wrap(&bar);

    try testing.expect(plusOne(wrappedFoo) == 13);
    try testing.expect(@TypeOf(plusOne(wrappedFoo)) == usize);
    try testing.expect(plusOne(wrappedBar) == 11.5);
    try testing.expect(@TypeOf(plusOne(wrappedBar)) == f32);
}
