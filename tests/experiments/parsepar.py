#!/usr/bin/env python3

import unittest

def parse_open (data, offset):

    if offset < len (data) and data[offset] == "(":
        return (True, offset + 1)
    return (False, offset)

def parse_close (data, offset):

    if offset < len (data) and data[offset] == ")":
        return (True, offset + 1)
    return (False, offset)

class Snapshot:

    def __init__ (self, stage, found, offset):
        self.stage  = stage
        self.found  = found
        self.offset = offset

def parse_iterative (data, offset = 0):

    result = True
    stack  = []

    stack.append (Snapshot (stage=0, found=False, offset=offset))

    while stack:

        frame = stack.pop ()

        if frame.stage == 0:

            (frame.found, tmp_offset) = parse_open (data, frame.offset)
            if not frame.found:
                result = (False, frame.offset)
            else:
                stack.append (Snapshot (stage=1, found=True, offset=tmp_offset))

        elif frame.stage == 1:

            stack.append (Snapshot (stage=2, found=True, offset=frame.offset))
            if frame.found:
                stack.append (Snapshot (stage=0, found=True, offset=frame.offset))

        elif frame.stage == 2:
            result = parse_close (data, frame.offset)

    return result

def parse_recursive (data, offset = 0):

    (found, offset) = parse_open (data, offset)
    if not found:
        return (False, offset)

    while found:
        (found, offset) = parse_recursive (data, offset)

    return parse_close (data, offset)

def parse_recursive_inner (data, offset, found):

    if not found:
        return parse_close (data, offset)

    (found, offset) = parse_recursive2 (data, offset)

    return parse_recursive_inner (data, offset, found)


def parse_recursive2 (data, offset = 0):

    (found, offset) = parse_open (data, offset)
    if not found:
        return (False, offset)

    return parse_recursive_inner (data, offset, found)

def parse_recursive3 (data, offset = 0, found = False, stage = 0):

    if stage == 0:

        (found, offset) = parse_open (data, offset)
        if not found:
            return (False, offset)

    elif stage == 1:

        if not found:
            return parse_close (data, offset)

        (found, offset) = parse_recursive3 (data, offset)

    return parse_recursive3 (data, offset, found, stage=1)

class State:
    def __init__ (self, offset, found, stage):
        self.offset = offset
        self.found  = found
        self.stage  = stage

def parse_recursive4 (data, param_offset=0, param_found=False, param_stage = 0):

    retval = (False, 0)
    stack = []
    stack.append (State (param_offset, param_found, param_stage))

    while stack:

        state = stack.pop()

        if state.stage == 0:

            (loc_found, loc_offset) = parse_open (data, state.offset)
            if not loc_found:
                retval = (False, loc_offset)
            else:
                stack.append (State (loc_offset, loc_found, 1))

        elif state.stage == 1:

            if not state.found:
                retval = parse_close (data, state.offset)
            else:
                stack.append (State (0, 0, 2))
                stack.append (State (state.offset, False, 0))

        elif state.stage == 2:
            stack.append (State (retval[1], retval[0], 1))

    return retval

class TestParser(unittest.TestCase):

    def test_simple1(self):
        self.assertEqual (parse_recursive ("()"), (True, 2))

    def test_simple2(self):
        self.assertEqual (parse_recursive ("(())"), (True, 4))

    def test_simple3(self):
        self.assertEqual (parse_recursive ("(()())"), (True, 6))

    def test_simple9(self):
        self.assertEqual (parse_recursive ("(()()()(((((()))))()))"), (True, 22))

    def test_fail(self):
        self.assertEqual (parse_recursive ("(()(((((()))))())("), (False, 18))

    def test_fail_short1(self):
        self.assertEqual (parse_recursive ("("), (False, 1))

    def test_fail_short2(self):
        self.assertEqual (parse_recursive (")"), (False, 0))

    def test_fail_unrelated(self):
        self.assertEqual (parse_recursive ("This is a test"), (False, 0))

    def test_simple1_r(self):
        self.assertEqual (parse_recursive2 ("()"), (True, 2))

    def test_simple2_r(self):
        self.assertEqual (parse_recursive2 ("(())"), (True, 4))

    def test_simple3_r(self):
        self.assertEqual (parse_recursive2 ("(()())"), (True, 6))

    def test_simple9_r(self):
        self.assertEqual (parse_recursive2 ("(()()()(((((()))))()))"), (True, 22))

    def test_fail_r(self):
        self.assertEqual (parse_recursive2 ("(()(((((()))))())("), (False, 18))

    def test_fail_short1_r(self):
        self.assertEqual (parse_recursive2 ("("), (False, 1))

    def test_fail_short2_r(self):
        self.assertEqual (parse_recursive2 (")"), (False, 0))

    def test_fail_unrelated_r(self):
        self.assertEqual (parse_recursive2 ("This is a test"), (False, 0))

    def test_simple1_r3(self):
        self.assertEqual (parse_recursive3 ("()"), (True, 2))

    def test_simple2_r3(self):
        self.assertEqual (parse_recursive3 ("(())"), (True, 4))

    def test_simple3_r3(self):
        self.assertEqual (parse_recursive3 ("(()())"), (True, 6))

    def test_simple9_r3(self):
        self.assertEqual (parse_recursive3 ("(()()()(((((()))))()))"), (True, 22))

    def test_fail_r3(self):
        self.assertEqual (parse_recursive3 ("(()(((((()))))())("), (False, 18))

    def test_fail_short1_r3(self):
        self.assertEqual (parse_recursive3 ("("), (False, 1))

    def test_fail_short2_r3(self):
        self.assertEqual (parse_recursive3 (")"), (False, 0))

    def test_fail_unrelated_r3(self):
        self.assertEqual (parse_recursive3 ("This is a test"), (False, 0))

    def test_simple1_r4(self):
        self.assertEqual (parse_recursive4 ("()"), (True, 2))

    def test_simple2_r4(self):
        self.assertEqual (parse_recursive4 ("(())"), (True, 4))

    def test_simple3_r4(self):
        self.assertEqual (parse_recursive4 ("(()())"), (True, 6))

    def test_simple9_r4(self):
        self.assertEqual (parse_recursive4 ("(()()()(((((()))))()))"), (True, 22))

    def test_fail_r4(self):
        self.assertEqual (parse_recursive4 ("(()(((((()))))())("), (False, 18))

    def test_fail_short1_r4(self):
        self.assertEqual (parse_recursive4 ("("), (False, 1))

    def test_fail_short2_r4(self):
        self.assertEqual (parse_recursive4 (")"), (False, 0))

    def test_fail_unrelated_r4(self):
        self.assertEqual (parse_recursive4 ("This is a test"), (False, 0))

if __name__ == '__main__':
    unittest.main()
