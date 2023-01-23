const std = @import("std");
const testing = std.testing;
const Any = @import("generic.zig").Any;

const Type = std.builtin.Type;
const EnumField = Type.EnumField;
const meta = std.meta;
const trait = meta.trait;
const is = trait.is;

fn enumsCompatible(comptime A: type, comptime B: type) bool {
    if (!is(.Enum)(A) or !is(.Enum)(B)) @compileError("Expected enums");

    const layoutsMatch = meta.containerLayout(A) == meta.containerLayout(B);
    const exhaustiveMatch = @typeInfo(A).Enum.is_exhaustive == @typeInfo(B).Enum.is_exhaustive;
    return layoutsMatch and exhaustiveMatch;
}

fn expectIdenticalEnums(comptime A: type, comptime B: type) !void {
    comptime {
        if (!enumsCompatible(A, B)) return error.TestUnexpectedResult;
        var result = @typeInfo(A).Enum.tag_type == @typeInfo(B).Enum.tag_type;
        result = result and meta.fields(A).len == meta.fields(B).len;
        result = result and blk: for (meta.fields(A)) |field, i| {
            if (!std.mem.eql(u8, meta.fields(B)[i].name, field.name)) break :blk false;
            if (meta.fields(B)[i].value != field.value) break :blk false;
        } else break :blk true;
        if (!result) return error.TestUnexpectedResult;
    }
}

pub const Options = struct {
    tag: TagRule = .{ .predefined = .equal },
    tag_arg: ?Any = null,
    value: ValueRule = .{ .predefined = .equal },
    value_arg: ?Any = null,
};

const TagRule = union(enum) {
    predefined: PredefinedRules,
    custom: TagRuleFn,

    pub fn get(comptime self: TagRule, comptime a: type, comptime b: type, comptime arg: ?Any) type {
        return switch (self) {
            .predefined => self.predefined.func()(a, b, arg),
            .custom => self.custom(a, b, arg),
        };
    }

    const TagRuleFn = *const fn (a: type, b: type, comptime arg: ?Any) type;

    const PredefinedRules = enum {
        auto,
        equal,
        first,
        second,
        exact,

        pub fn func(comptime e: PredefinedRules) TagRuleFn {
            return @field(PredefinedRules, @tagName(e) ++ "Fn");
        }

        pub fn autoFn(comptime _: type, comptime _: type, comptime arg: ?Any) type {
            if (!trait.hasFields(arg.?.T, .{ "min", "max" }) or !is(.Struct)(arg.?.T)) {
                @compileError("arg must contain struct with fields {min: comptime_int, max: comptime_int} got '" ++ @typeName(arg.?.T) ++ "' instead");
            }
            const minMax = arg.?.unwrap().*;
            return std.math.IntFittingRange(minMax.min, minMax.max);
        }

        pub fn equalFn(comptime a: type, comptime b: type, comptime _: ?Any) type {
            if (a != b) @compileError("Expected equal enum tag types");
            return a;
        }

        pub fn firstFn(comptime a: type, comptime _: type, comptime _: ?Any) type {
            return a;
        }

        pub fn secondFn(comptime _: type, comptime b: type, comptime _: ?Any) type {
            return b;
        }

        pub fn exactFn(comptime _: type, comptime _: type, comptime arg: ?Any) type {
            if (arg.?.T != type) {
                @compileError("arg must contain type, got '" ++ @typeName(arg.?.T) ++ "' instead");
            }
            return arg.?.unwrap().*;
        }
    };
};

const ValueRule = union(enum) {
    predefined: PredefinedRules,
    custom: ValueRuleFn,

    const ValueRuleFn = *const fn (comptime a: EnumField, comptime b: EnumField, comptime arg: ?Any) comptime_int;

    pub fn get(comptime self: ValueRule, comptime a: EnumField, comptime b: EnumField, comptime arg: ?Any) comptime_int {
        return switch (self) {
            .predefined => self.predefined.func()(a, b, arg),
            .custom => self.custom(a, b, arg),
        };
    }

    const PredefinedRules = enum {
        auto,
        equal,
        first,
        second,
        min,
        max,

        pub fn func(comptime e: PredefinedRules) ValueRuleFn {
            return @field(PredefinedRules, @tagName(e) ++ "Fn");
        }

        pub fn autoFn(comptime _: EnumField, comptime _: EnumField, comptime arg: ?Any) comptime_int {
            if (arg.?.T != comptime_int) {
                @compileError("arg must contain comptime_int, got " ++ @typeName(arg.?.T) ++ " instead");
            }
            var i: *comptime_int = arg.?.unwrap();
            defer i.* += 1;
            return i.*;
        }

        pub fn equalFn(comptime a: EnumField, comptime b: EnumField, comptime _: ?Any) comptime_int {
            if (a.value != b.value) @compileError("Expected equal enum field values");
            return a.value;
        }

        pub fn firstFn(comptime a: EnumField, comptime _: EnumField, comptime _: ?Any) comptime_int {
            return a.value;
        }

        pub fn secondFn(comptime _: EnumField, comptime b: EnumField, comptime _: ?Any) comptime_int {
            return b.value;
        }

        pub fn minFn(comptime a: EnumField, comptime b: EnumField, comptime _: ?Any) comptime_int {
            return @min(a.value, b.value);
        }

        pub fn maxFn(comptime a: EnumField, comptime b: EnumField, comptime _: ?Any) comptime_int {
            return @max(a.value, b.value);
        }
    };
};

/// Set **intersection** operation on enums `A ∩ B`
pub fn And(comptime A: type, comptime B: type, comptime options: Options) type {
    if (!enumsCompatible(A, B)) @compileError("Enums are incompatible");

    var C: Type = .{ .Enum = .{
        .layout = meta.containerLayout(A),
        .tag_type = u0,
        .fields = &.{},
        .decls = &.{},
        .is_exhaustive = @typeInfo(A).Enum.is_exhaustive,
    } };

    var i = 0;
    var values: []const comptime_int = &.{};

    for (meta.fields(A)) |field| {
        const BIndex = meta.fieldIndex(B, field.name) orelse continue;
        const tagArg = if (options.value == .predefined and options.value.predefined == .auto) Any.wrap(&i) else options.value_arg;
        const value = options.value.get(field, meta.fields(B)[@as(usize, BIndex)], tagArg);
        if (std.mem.count(comptime_int, values, &.{value}) > 0)
            @compileError("Enum field '" ++ value.name ++ "' value duplicated, use another rule or edit your enums");
        values = values ++ [_]comptime_int{value};
        C.Enum.fields = C.Enum.fields ++ [_]EnumField{.{ .name = field.name, .value = value }};
    }

    const isAutoValue = options.tag == .predefined and options.tag.predefined == .auto;
    const tagArg = if (isAutoValue) Any.wrap(&std.mem.minMax(comptime_int, values)) else options.tag_arg;

    C.Enum.tag_type = options.tag.get(meta.Tag(A), meta.Tag(B), tagArg);

    return @Type(C);
}

test "enum AND" {
    {
        const A = enum(u8) { a = 0, b = 1, c = 2 };
        const B = enum(u8) { b = 1, c = 2, d = 3 };

        // default behaviour is to expect equal tags/values,
        // throw compile error otherwise
        // (u8){a=0, b=1, c=2} ∩ (u8){b=1, c=2, d=3} = (u8){b=1, c=2}
        const CDefault = And(A, B, .{});
        try expectIdenticalEnums(CDefault, enum(u8) { b = 1, c });
    }

    const A = enum(u5) { a = 1, b = 2, c = 3 };
    const B = enum(u7) { b = 1, c = 2, d = 3 };

    // auto value/tag fill (same as untagged)
    // (u5){a=1, b=2, c=3} ∩ (u7){b=1, c=2, d=3} = {b, c}
    const CAuto = And(A, B, .{ .tag = .{ .predefined = .auto }, .value = .{ .predefined = .auto } });
    try expectIdenticalEnums(CAuto, enum { b, c });

    // pick first tag/value
    // (u5){a=1, b=2, c=3} ∩ (u7){b=1, c=2, d=3} = (u5){b=2, c=3}
    const CFirst = And(A, B, .{ .tag = .{ .predefined = .first }, .value = .{ .predefined = .first } });
    try expectIdenticalEnums(CFirst, enum(u5) { b = 2, c });

    // pick second tag/value
    // (u5){a=1, b=2, c=3} ∩ (u7){b=1, c=2, d=3} = (u7){b=1, c=2}
    const CSecond = And(A, B, .{ .tag = .{ .predefined = .second }, .value = .{ .predefined = .second } });
    try expectIdenticalEnums(CSecond, enum(u7) { b = 1, c });

    // pick exact tag (u8), min value
    // (u5){a=1, b=2, c=3} ∩ (u7){b=1, c=2, d=3} = (u8){b=1, c=2}
    const T = u8;
    const CExactMin = And(A, B, .{ .tag = .{ .predefined = .exact }, .tag_arg = Any.wrap(&T), .value = .{ .predefined = .min } });
    try expectIdenticalEnums(CExactMin, enum(T) { b = 1, c });

    // pick exact tag (u8), max value
    // (u5){a=1, b=2, c=3} ∩ (u7){b=1, c=2, d=3} = (u8){b=2, c=3}
    const CExactMax = And(A, B, .{ .tag = .{ .predefined = .exact }, .tag_arg = Any.wrap(&T), .value = .{ .predefined = .max } });
    try expectIdenticalEnums(CExactMax, enum(T) { b = 2, c });
}

/// Set **intersection** operation on enums `A ∩ B`, enums treated as untagged
pub fn AndUntagged(comptime A: type, comptime B: type) type {
    const options = Options{
        .tag = .{ .predefined = .auto },
        .value = .{ .predefined = .auto },
    };

    return And(A, B, options);
}

test "enum AND untagged" {
    const A = enum { a, b, c };
    const B = enum { b, c, d };

    // {a, b, c} ∩ {b, c, d} = {b, c}
    const C = AndUntagged(A, B);
    try expectIdenticalEnums(C, enum { b, c });
}

/// Set **union** operation on enums `A ∪ B`
pub fn Or(comptime A: type, comptime B: type, comptime options: Options) type {
    if (!enumsCompatible(A, B)) @compileError("Enums are incompatible");

    var C: Type = .{ .Enum = .{
        .layout = meta.containerLayout(A),
        .tag_type = u0,
        .fields = &.{},
        .decls = &.{},
        .is_exhaustive = @typeInfo(A).Enum.is_exhaustive,
    } };

    var i = 0;
    var values: []const comptime_int = &.{};

    for (meta.fields(A)) |fieldA| {
        const match = blk: {
            const BIndex = @as(usize, meta.fieldIndex(B, fieldA.name) orelse break :blk fieldA);
            break :blk meta.fields(B)[BIndex];
        };
        const tagArg = if (options.value == .predefined and options.value.predefined == .auto) Any.wrap(&i) else options.value_arg;
        const value = options.value.get(fieldA, match, tagArg);
        if (std.mem.count(comptime_int, values, &.{value}) > 0)
            @compileError("Enum field value duplicated, use another rule or edit your enums");
        values = values ++ [_]comptime_int{value};
        C.Enum.fields = C.Enum.fields ++ [_]EnumField{.{ .name = fieldA.name, .value = value }};
    }

    for (meta.fields(B)) |fieldB| {
        if (@hasField(A, fieldB.name)) continue;
        const tagArg = if (options.value == .predefined and options.value.predefined == .auto) Any.wrap(&i) else options.value_arg;
        const value = options.value.get(fieldB, fieldB, tagArg);
        if (std.mem.count(comptime_int, values, &.{value}) > 0)
            @compileError("Enum field value duplicated, use another rule or edit your enums");
        values = values ++ [_]comptime_int{value};
        C.Enum.fields = C.Enum.fields ++ [_]EnumField{.{ .name = fieldB.name, .value = value }};
    }

    const isAutoValue = options.tag == .predefined and options.tag.predefined == .auto;
    const tagArg = if (isAutoValue) Any.wrap(&std.mem.minMax(comptime_int, values)) else options.tag_arg;

    C.Enum.tag_type = options.tag.get(meta.Tag(A), meta.Tag(B), tagArg);

    return @Type(C);
}

test "enum OR" {
    {
        const A = enum(u8) { a = 1, b = 2 };
        const B = enum(u8) { b = 2, c = 3 };

        // default behaviour is to expect equal tags/values,
        // throw compile error otherwise
        // (u8){a=1, b=2} ∪ (u8){b=2, c=3} = (u8){a=1, b=2, c=3}
        const CDefault = Or(A, B, .{});
        try expectIdenticalEnums(CDefault, enum(u8) { a = 1, b, c });
    }

    const A = enum(u5) { a = 1, b = 2 };
    const B = enum(u7) { b = 4, c = 3 };

    // auto value/tag fill (same as untagged)
    // (u5){a=1, b=2} ∪ (u7){b=4, c=3} = {a, b, c}
    const CAuto = Or(A, B, .{ .tag = .{ .predefined = .auto }, .value = .{ .predefined = .auto } });
    try expectIdenticalEnums(CAuto, enum { a, b, c });

    // pick first tag/value
    // (u5){a=1, b=2} ∪ (u7){b=4, c=3} = (u5){a=1, b=2, c=3}
    const CFirst = Or(A, B, .{ .tag = .{ .predefined = .first }, .value = .{ .predefined = .first } });
    try expectIdenticalEnums(CFirst, enum(u5) { a = 1, b, c });

    // pick second tag/value
    // (u5){a=1, b=2} ∪ (u7){b=4, c=3} = (u7){a=1, b=4, c=3}
    const CSecond = Or(A, B, .{ .tag = .{ .predefined = .second }, .value = .{ .predefined = .second } });
    try expectIdenticalEnums(CSecond, enum(u7) { a = 1, b = 4, c = 3 });

    // pick exact tag (u8), min value
    // (u5){a=1, b=2} ∪ (u7){b=4, c=3} = (u8){a=1, b=2, c=3}
    const T = u8;
    const CExactMin = Or(A, B, .{ .tag = .{ .predefined = .exact }, .tag_arg = Any.wrap(&T), .value = .{ .predefined = .min } });
    try expectIdenticalEnums(CExactMin, enum(T) { a = 1, b, c });

    // pick exact tag (u8), max value
    // (u5){a=1, b=2} ∪ (u7){b=4, c=3} = (u8){a=1, b=4, c=3}
    const CExactMax = Or(A, B, .{ .tag = .{ .predefined = .exact }, .tag_arg = Any.wrap(&T), .value = .{ .predefined = .max } });
    try expectIdenticalEnums(CExactMax, enum(T) { a = 1, b = 4, c = 3 });
}

/// Set **union** operation on enums `A ∪ B`, enums treated as untagged
pub fn OrUntagged(comptime A: type, comptime B: type) type {
    const options = Options{
        .tag = .{ .predefined = .auto },
        .value = .{ .predefined = .auto },
    };

    return Or(A, B, options);
}

test "enum OR untagged" {
    const A = enum { a, b };
    const B = enum { b, c };

    // {a, b} ∪ {b, c} = {a, b, c}
    const C = OrUntagged(A, B);
    try expectIdenticalEnums(C, enum { a, b, c });
}

/// Set **difference** operation on enums `A \ B`
pub fn Diff(comptime A: type, comptime B: type, comptime options: Options) type {
    if (options.value == .predefined) {
        switch (options.value.predefined) {
            .min, .max, .second => |tag| @compileError("Can't use value rule '" ++ @tagName(tag) ++ "' with Diff operation"),
            else => {},
        }
    }
    if (!enumsCompatible(A, B)) @compileError("Enums are incompatible");

    var C: Type = .{ .Enum = .{
        .layout = meta.containerLayout(A),
        .tag_type = u0,
        .fields = &.{},
        .decls = &.{},
        .is_exhaustive = @typeInfo(A).Enum.is_exhaustive,
    } };

    var i = 0;
    var values: []const comptime_int = &.{};

    for (meta.fields(A)) |field| {
        if (@hasField(B, field.name)) continue;
        const tagArg = if (options.value == .predefined and options.value.predefined == .auto) Any.wrap(&i) else options.value_arg;
        const value = options.value.get(field, field, tagArg);
        if (std.mem.count(comptime_int, values, &.{value}) > 0)
            @compileError("Enum field '" ++ value.name ++ "' value duplicated, use another rule or edit your enums");
        values = values ++ [_]comptime_int{value};
        C.Enum.fields = C.Enum.fields ++ [_]EnumField{.{ .name = field.name, .value = value }};
    }

    const isAutoValue = options.tag == .predefined and options.tag.predefined == .auto;
    const tagArg = if (isAutoValue) Any.wrap(&std.mem.minMax(comptime_int, values)) else options.tag_arg;

    C.Enum.tag_type = options.tag.get(meta.Tag(A), meta.Tag(B), tagArg);

    return @Type(C);
}

test "enum difference" {
    {
        const A = enum(u8) { a = 1, b = 2, c = 3 };
        const B = enum(u8) { c = 1, d = 2, e = 3 };

        // default behaviour is to expect equal tags/values,
        // throw compile error otherwise; in case of diff operation
        // second comparison is irrelevant
        // (u8){a=1, b=2, c=3} \ (u8){c=1, d=2, e=3} = (u8){a=1, b=2}
        const CDefault = Diff(A, B, .{});
        try expectIdenticalEnums(CDefault, enum(u8) { a = 1, b });
    }

    const A = enum(u5) { a = 1, b = 2, c = 3 };
    const B = enum(u7) { c = 1, d = 2, e = 3 };

    // auto value/tag fill (same as untagged)
    // (u5){a=1, b=2, c=3} \ (u7){c=1, d=2, e=3} = {a, b}
    const CAuto = Diff(A, B, .{ .tag = .{ .predefined = .auto }, .value = .{ .predefined = .auto } });
    try expectIdenticalEnums(CAuto, enum { a, b });

    // pick first tag/value
    // (u5){a=1, b=2, c=3} \ (u7){c=1, d=2, e=3} = (u5){a=1, b=2}
    const CFirst = Diff(A, B, .{ .tag = .{ .predefined = .first }, .value = .{ .predefined = .first } });
    try expectIdenticalEnums(CFirst, enum(u5) { a = 1, b });

    // picking second/min/max value doesn't make sense for diff operation

    // pick second tag, auto value
    // (u5){a=1, b=2, c=3} \ (u7){c=1, d=2, e=3} = (u7){a, b}
    const CSecond = Diff(A, B, .{ .tag = .{ .predefined = .second }, .value = .{ .predefined = .auto } });
    try expectIdenticalEnums(CSecond, enum(u7) { a, b });

    // pick exact tag (u4), auto value
    // (u5){a=1, b=2, c=3} \ (u7){c=1, d=2, e=3} = (u4){a, b}
    const T = u4;
    const CExact = Diff(A, B, .{ .tag = .{ .predefined = .exact }, .tag_arg = Any.wrap(&T), .value = .{ .predefined = .auto } });
    try expectIdenticalEnums(CExact, enum(u4) { a, b }); // T instead of u4 results in segfault?

}

/// Set **difference** operation on enums `A \ B`, enums treated as untagged
pub fn DiffUntagged(comptime A: type, comptime B: type) type {
    const options = Options{
        .tag = .{ .predefined = .auto },
        .value = .{ .predefined = .auto },
    };

    return Diff(A, B, options);
}

test "enum difference untagged" {
    const A = enum { a, b, c };
    const B = enum { c, d, e };

    // {a, b, c} \ {c, d, e} = {a, b}
    const C = DiffUntagged(A, B);
    try expectIdenticalEnums(C, enum { a, b });
}

/// Set **symmetric difference** operation on enums `A △ B = (A \ B) ∪ (B \ A)`, also denoted as `A ⊖ B`
pub fn Xor(comptime A: type, comptime B: type, comptime options: Options) type {
    var opts = options;
    if (opts.tag == .predefined and opts.tag.predefined == .second) opts.tag = .{ .predefined = .first };
    return Or(
        Diff(A, B, options),
        Diff(B, A, options),
        opts,
    );
}

test "enum XOR" {
    {
        const A = enum(u8) { a = 1, b = 2, c = 3 };
        const B = enum(u8) { b = 1, c = 2, d = 3 };

        // default behaviour is to expect equal tags/values,
        // throw compile error otherwise; in case of XOR operation
        // second comparison is irrelevant
        // (u8){a=1, b=2, c=3} △ (u8){b=1, c=2, d=3} = (u8){a=1, d=3}
        const CDefault = Xor(A, B, .{});
        try expectIdenticalEnums(CDefault, enum(u8) { a = 1, d = 3 });
    }

    const A = enum(u5) { a = 1, b = 2, c = 3 };
    const B = enum(u7) { b = 1, c = 2, d = 3 };

    // auto value/tag fill (same as untagged)
    // (u5){a=1, b=2, c=3} △ (u7){b=1, c=2, d=3} = {a, d}
    const CAuto = Xor(A, B, .{ .tag = .{ .predefined = .auto }, .value = .{ .predefined = .auto } });
    try expectIdenticalEnums(CAuto, enum { a, d });

    // pick first tag/value
    // (u5){a=1, b=2, c=3} △ (u7){b=1, c=2, d=3} = (u5){a=1, d=3}
    const CFirst = Xor(A, B, .{ .tag = .{ .predefined = .first }, .value = .{ .predefined = .first } });
    try expectIdenticalEnums(CFirst, enum(u5) { a = 1, d = 3 });

    // picking second/min/max value doesn't make sense for XOR operation

    // pick second tag, auto value
    // (u5){a=1, b=2, c=3} △ (u7){b=1, c=2, d=3} = (u7){a, d}
    const CSecond = Xor(A, B, .{ .tag = .{ .predefined = .second }, .value = .{ .predefined = .auto } });
    try expectIdenticalEnums(CSecond, enum(u7) { a, d });

    // pick exact tag, auto value
    // (u5){a=1, b=2, c=3} △ (u7){b=1, c=2, d=3} = (u4){a, d}
    const T = u4;
    const CExact = Xor(A, B, .{ .tag = .{ .predefined = .exact }, .tag_arg = Any.wrap(&T), .value = .{ .predefined = .auto } });
    try expectIdenticalEnums(CExact, enum(u4) { a, d });
}

/// Set **symmetric difference** operation on enums `A △ B = (A \ B) ∪ (B \ A)`, also denoted as `A ⊖ B`,
/// enums treated as untagged
pub fn XorUntagged(comptime A: type, comptime B: type) type {
    const options = Options{
        .tag = .{ .predefined = .auto },
        .value = .{ .predefined = .auto },
    };

    return Xor(A, B, options);
}

test "enum XOR untagged" {
    const A = enum { a, b, c };
    const B = enum { c, d, e };

    // {a, b, c} △ {c, d, e} = {a, b, d, e}
    const C = XorUntagged(A, B);
    try expectIdenticalEnums(C, enum { a, b, d, e });
}

test "custom rule" {
    // We have two enums that we want to merge, the example is silly, but bear with me
    const A = enum(u8) {
        apple = 10,
        banana = 4,
        Cherry = 7,
        date = 8,
    };
    const B = enum(u8) {
        Cherry = 6,
        date = 3,
        elderberry = 5,
        fig = 12,
    };

    // The values of enum fields are
    // 1. explicitly defined, so we would want to keep them
    // 2. do not match between enums, so we must resolve this

    // Let's use custom rule to pick the right value! The rule will be as follows:
    // if the enum field name starts with uppercase letter use the max possible value, else use min.
    // Custom value rule function signature is
    // fn (comptime a: EnumField, comptime b: EnumField, comptime arg: ?Any) comptime_int
    // const EnumField = struct {name: [:0]const u8, value: comptime_int};
    const S = struct {
        fn customRule(comptime a: EnumField, comptime b: EnumField, comptime _: ?Any) comptime_int {
            return if (a.name[0] >= 'A' and a.name[0] <= 'Z') @max(a.value, b.value) else @min(a.value, b.value);
        }
    };

    // (u8){apple=10, banana=4, Cherry=7, date=8}
    // ∩
    // (u8){Cherry=6, date=3, elderberry=5, fig=12}
    // =
    // (u8){Cherry=7, date=3}
    const AndC = And(A, B, .{ .value = .{ .custom = S.customRule } });
    try expectIdenticalEnums(AndC, enum(u8) { Cherry = 7, date = 3 });

    const AndInfo = @typeInfo(AndC).Enum;
    // The AND result is an enum with two fields
    try testing.expect(AndInfo.fields.len == 2);
    // for capitalized field value is @max(7, 6) == 7
    try testing.expect(@enumToInt(AndC.Cherry) == @max(@enumToInt(A.Cherry), @enumToInt(B.Cherry)));
    try testing.expect(@enumToInt(AndC.Cherry) == 7);
    // for non-capitalized field value is @min(8, 3) == 3
    try testing.expect(@enumToInt(AndC.date) == @min(@enumToInt(A.date), @enumToInt(B.date)));
    try testing.expect(@enumToInt(AndC.date) == 3);

    // (u8){apple=10, banana=4, Cherry=7, date=8}
    // ∪
    // (u8){Cherry=6, date=3, elderberry=5, fig=12}
    // =
    // (u8){apple=10, banana=4, Cherry=7, date=3, elderberry=5, fig=12}
    const OrC = Or(A, B, .{ .value = .{ .custom = S.customRule } });
    try expectIdenticalEnums(OrC, enum(u8) { apple = 10, banana = 4, Cherry = 7, date = 3, elderberry = 5, fig = 12 });

    const OrInfo = @typeInfo(OrC).Enum;
    // The OR result is an enum with six fields
    try testing.expect(OrInfo.fields.len == 6);
    // for capitalized field value is @max(7, 6) == 7
    try testing.expect(@enumToInt(OrC.Cherry) == @max(@enumToInt(A.Cherry), @enumToInt(B.Cherry)));
    try testing.expect(@enumToInt(OrC.Cherry) == 7);
    // for non-capitalized field value is @min(8, 3) == 3
    try testing.expect(@enumToInt(OrC.date) == @min(@enumToInt(A.date), @enumToInt(B.date)));
    try testing.expect(@enumToInt(OrC.date) == 3);
    // other fields are preserved from parents
    try testing.expect(@enumToInt(OrC.apple) == 10);
    try testing.expect(@enumToInt(OrC.banana) == 4);
    try testing.expect(@enumToInt(OrC.elderberry) == 5);
    try testing.expect(@enumToInt(OrC.fig) == 12);
}
