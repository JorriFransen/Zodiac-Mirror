

program = """
Vec2i_(?) :: struct 
{
    x : int;
    y : int;
}

test_func(?) :: ()
{
    c.printf("test (?) \\n");
    c.printf("hi (?) \\n");

    v : Vec2i_(?) = { (?), (?) * 2 };
    print_vec2i_(?)(v);
}

print_vec2i_(?) :: (v: Vec2i_(?))
{
    c.printf("{ %lu, %lu }\\n", v.x, v.y);
}
"""

main = """

c :: import c;

main :: ()
{
    c.printf("main\\n");
    (?)
}
"""

test_func_calls = ""
for i in range (5000):
    print(program.replace("(?)", str(i)))
    test_func_calls += "test_func{0}();\n".format(str(i))

print(main.replace("(?)", test_func_calls))