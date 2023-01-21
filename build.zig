const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    // Standard release options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    const mode = b.standardReleaseOptions();

    const lib = b.addStaticLibrary("zeta-p", "src/main.zig");
    lib.setBuildMode(mode);
    lib.install();

    const generic_tests = b.addTest("src/generic.zig");
    generic_tests.setBuildMode(mode);

    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&generic_tests.step);
}