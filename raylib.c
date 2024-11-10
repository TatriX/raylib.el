// NOTES
// See https://phst.eu/emacs-modules

// TODO:
// - use `make_global_ref` for `nil` and the like?

#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <raylib.h>
#include <rlgl.h>

#include <emacs-module.h>

int plugin_is_GPL_compatible;

bool has_window;

#define S(s) (env->intern(env, s))

// NOTE: This should be equal to MAX_TEXT_BUFFER_LENGTH
char global_text_buffer[1024];

typedef struct string {
    char *text;
    ptrdiff_t size;
} string;

// NOTE: emacs values *must* not be stored between calls.
#define nil S("nil")
#define t S("t")

#define exit_check() if (env->non_local_exit_check(env) != emacs_funcall_exit_return) return nil;

#define is_nil(value) (!env->is_not_nil(env, value))
#define eq(a, b) env->eq(env, a, b)

#define call(name, n, ...) env->funcall(env, S(name), n, (emacs_value[]){ __VA_ARGS__ })

#define get_int(value) extract_int(env, value)
#define get_float(value) extract_float(env, value)
#define get_string(value) extract_string(env, value)

#define aref(vec, index) env->vec_get(env, vec, index)
#define slot_value(object, slot) call("slot-value", 2, object, S(slot))

#define get_vector2(value) extract_vector2(env, value)
#define get_color(value) extract_color(env, value)
#define get_rectangle(value) extract_rectangle(env, value)
#define get_camera_2d(value) extract_camera_2d(env, value)

// NOTE:IMPORTANT: Be careful when using all of these macros, make
// sure you don't cause multiple evaluation of arguments.
#define new_int(value) env->make_integer(env, value)
#define new_vector2(value) call("vector", 2, new_float(value.x), new_float(value.y));
#define new_float(value) env->make_float(env, value)
#define new_color(color) make_color(env, color)

// NOTE: signal() must be called with string literals!
#define signal(error_symbol, error_string) call_signal(env, error_symbol, error_string, sizeof(error_string) - 1)

// NOTE: message() must be called with string literals!
#define message(string) call_message(env, string, sizeof(string) - 1)

static void
call_message(emacs_env *env, const char *text, int text_len) {
    emacs_value string = env->make_string(env, text, text_len);
    call("message", 1, string);
}

static void
call_signal(emacs_env *env, const char *error_symbol, const char *error_string, int error_string_len) {
    emacs_value message = env->make_string(env, error_string, error_string_len);
    emacs_value data = env->funcall(env, S("list"), 1, &message);
    env->non_local_exit_signal(env, S(error_symbol), data);
}

// NOTE: free_string() must be called on the returned value
static inline string
extract_string(emacs_env *env, emacs_value value) {
    struct string result;
    env->copy_string_contents(env, value, NULL, &result.size);

    // if the size is too big, most likely value is *not* a string
    assert(result.size < 1024*1024*1024);

    if (result.size <= sizeof(global_text_buffer)) {
        result.text = global_text_buffer;
    }  else {
        result.text = malloc(result.size);
        assert(result.text);
    }

    env->copy_string_contents(env, value, result.text, &result.size);
    return result;
}

static inline void
free_string(struct string *string) {
    if (string->size > sizeof(global_text_buffer)) {
        free(string->text);
    }
    *string = (struct string){};
}

[[maybe_unused]]
static void
print_type_of(emacs_env *env, emacs_value value) {
    emacs_value type = env->type_of(env, value);

    char *fmt = "type is %s";
    emacs_value string = env->make_string(env, fmt, sizeof(fmt));
    env->funcall(env, S("message"), 2, (emacs_value[]){string, type});

    /* emacs_value result = env->funcall(env, S("symbol-string"), 1, (emacs_value[]){type}); */
    /* string name = get_string(result); */
    /* printf("type-of: %s\n", name.text); */
    /* free_string(&name); */
}

static inline int
extract_int(emacs_env *env, emacs_value value) {
    if (is_nil(value)) {
        signal("wrong-type-argument", "expected int, got nil");
        return 0;
    }

    int result = env->extract_integer(env, value);

    if (env->non_local_exit_check(env) != emacs_funcall_exit_return) {
        env->non_local_exit_clear(env);
        result = env->extract_float(env, value);
    }

    if (env->non_local_exit_check(env) != emacs_funcall_exit_return) {
        env->non_local_exit_clear(env);
        // TODO: pass the value itself to `wrong-type-argument`
        signal("wrong-type-argument", "expected int");
        return 0;
    }

    return result;
}

static inline double
extract_float(emacs_env *env, emacs_value value) {
    if (is_nil(value)) {
        signal("wrong-type-argument", "expected float, got nil");
        return 0;
    }

    double result = env->extract_float(env, value);

    if (env->non_local_exit_check(env) != emacs_funcall_exit_return) {
        env->non_local_exit_clear(env);
        result = env->extract_integer(env, value);
    }

    if (env->non_local_exit_check(env) != emacs_funcall_exit_return) {
        env->non_local_exit_clear(env);
        signal("wrong-type-argument", "expected float");
        return 0;
    }

    return result;
}

static inline Vector2
extract_vector2(emacs_env *env, emacs_value value) {
    float x = get_float(aref(value, 0));
    float y = get_float(aref(value, 1));
    return (Vector2){x, y};
}

static inline Color
extract_color(emacs_env *env, emacs_value vec) {
    int size = env->vec_size(env, vec);
    if (size != 4) {
        signal("wrong-type-argument", "expected color");
        return (Color){};
    }

    Color color = {
        get_int(aref(vec, 0)),
        get_int(aref(vec, 1)),
        get_int(aref(vec, 2)),
        get_int(aref(vec, 3)),
    };
    return color;
}

static inline Rectangle
extract_rectangle(emacs_env *env, emacs_value vec) {
    int size = env->vec_size(env, vec);
    if (size != 4) {
        signal("wrong-type-argument", "expected rectangle");
        return (Rectangle){};
    }

    Rectangle rec = {
        get_float(aref(vec, 0)),
        get_float(aref(vec, 1)),
        get_float(aref(vec, 2)),
        get_float(aref(vec, 3)),
    };
    return rec;
}

static inline Camera2D
extract_camera_2d(emacs_env *env, emacs_value object) {
    Camera2D camera = {
        .offset = get_vector2(slot_value(object, "offset")),
        .target = get_vector2(slot_value(object, "target")),
        .rotation = get_float(slot_value(object, "rotation")),
        .zoom = get_float(slot_value(object, "zoom")),
    };
    return camera;
}

static inline emacs_value
make_color(emacs_env *env, Color color) {
    return call("rl-color", 4, new_int(color.r), new_int(color.g), new_int(color.b), new_int(color.a));
}


/// Exported functions

static emacs_value
rl_init_window(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr) {
    assert(n == 3);

    if (has_window) {
        signal("user-error", "window already exists");
        return nil;
    }

    int screen_width = env->extract_integer(env, args[0]);
    if (env->non_local_exit_check(env) != emacs_funcall_exit_return)
        return nil;
    int screen_height = env->extract_integer(env, args[1]);
    if (env->non_local_exit_check(env) != emacs_funcall_exit_return)
        return nil;

    string title = get_string(args[2]);
    InitWindow(screen_width, screen_height, title.text);
    has_window = true;

    free_string(&title);

    return nil;
}

static emacs_value
rl_close_window(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr) {
    if (has_window) {
        CloseWindow();
        has_window = false;
    }

    return nil;
}

static emacs_value
rl_window_should_close(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr) {
    if (WindowShouldClose()) {
        return t;
    }
    return nil;
}

static emacs_value
rl_get_frame_time(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr) {
    return env->make_float(env, GetFrameTime());
}

static emacs_value
rl_begin_drawing(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr) {
    BeginDrawing();
    return nil;
}

static emacs_value
rl_end_drawing(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr) {
    EndDrawing();
    return nil;
}

static emacs_value
rl_begin_mode_2d(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr) {
    assert(n == 1);
    Camera2D camera = get_camera_2d(args[0]);
    BeginMode2D(camera);
    return nil;
}

static emacs_value
rl_end_mode_2d(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr) {
    EndMode2D();
    return nil;
}

static emacs_value
rl_clear_background(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr) {
    assert(n == 1);
    Color color = extract_color(env, args[0]);
    ClearBackground(color);
    return nil;
}

static emacs_value
rl_draw_circle(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr) {
    assert(n == 4);

    int x = get_int(args[0]);
    int y = get_int(args[1]);
    int radius = get_int(args[2]);
    Color color = get_color(args[3]);

    DrawCircle(x, y, radius, color);

    return nil;
}

static emacs_value
rl_draw_circle_v(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr) {
    assert(n == 3);

    Vector2 v = get_vector2(args[0]);
    float radius = get_float(args[1]);
    Color color = get_color(args[2]);

    DrawCircleV(v, radius, color);

    return nil;
}

static emacs_value
rl_draw_rectangle(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr) {
    assert(n == 5);

    int posX = get_int(args[0]);
    int posY = get_int(args[1]);
    int width = get_int(args[2]);
    int height = get_int(args[3]);
    Color color = get_color(args[4]);

    DrawRectangle(posX, posY, width, height, color);

    return nil;
}

static emacs_value
rl_draw_rectangle_rec(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr) {
    assert(n == 2);

    Rectangle rec = get_rectangle(args[0]);
    Color color = get_color(args[1]);

    DrawRectangleRec(rec, color);

    return nil;
}

static emacs_value
rl_draw_rectangle_lines(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr) {
    assert(n == 5);

    int posX = get_int(args[0]);
    int posY = get_int(args[1]);
    int width = get_int(args[2]);
    int height = get_int(args[3]);
    Color color = get_color(args[4]);

    DrawRectangleLines(posX, posY, width, height, color);

    return nil;
}

static emacs_value
rl_draw_line(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr) {
    assert(n == 5);

    int startPosX = get_int(args[0]);
    int startPosY = get_int(args[1]);
    int endPosX = get_int(args[2]);
    int endPosY = get_int(args[3]);
    Color color = get_color(args[4]);

    DrawLine(startPosX, startPosY, endPosX, endPosY, color);

    return nil;
}

static emacs_value
rl_draw_line_v(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr) {
    assert(n == 3);

    Vector2 start = get_vector2(args[0]);
    Vector2 end = get_vector2(args[1]);
    Color color = get_color(args[2]);

    DrawLineV(start, end, color);

    return nil;
}

static emacs_value
rl_is_mouse_button_pressed(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr) {
    assert(n == 1);
    int key = get_int(args[0]);
    return IsMouseButtonPressed(key) ? t : nil;
}

static emacs_value
rl_is_mouse_button_down(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr) {
    assert(n == 1);
    int key = get_int(args[0]);
    return IsMouseButtonDown(key) ? t : nil;
}

static emacs_value
rl_is_mouse_button_released(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr) {
    assert(n == 1);
    int key = get_int(args[0]);
    return IsMouseButtonReleased(key) ? t : nil;
}

static emacs_value
rl_is_mouse_button_up(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr) {
    assert(n == 1);
    int key = get_int(args[0]);
    return IsMouseButtonUp(key) ? t : nil;
}

static emacs_value
rl_get_mouse_position(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr) {
    assert(n == 0);
    Vector2 position = GetMousePosition();
    return new_vector2(position)
}

static emacs_value
rl_get_mouse_delta(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr) {
    assert(n == 0);
    Vector2 delta = GetMouseDelta();
    return new_vector2(delta);
}

static emacs_value
rl_get_mouse_wheel_move(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr) {
    assert(n == 0);
    float move = GetMouseWheelMove();
    return new_float(move);
}

static emacs_value
rl_get_screen_to_world_2d(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr) {
    assert(n == 2);

    Vector2 position = get_vector2(args[0]);
    Camera2D camera = get_camera_2d(args[1]);

    Vector2 result = GetScreenToWorld2D(position, camera);
    return new_vector2(result);
}

static emacs_value
rl_is_key_down(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr) {
    assert(n == 1);
    int key = get_int(args[0]);
    return IsKeyDown(key) ? t : nil;
}

static emacs_value
rl_is_key_up(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr) {
    assert(n == 1);
    int key = get_int(args[0]);
    return IsKeyUp(key) ? t : nil;
}

static emacs_value
rl_is_key_pressed(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr) {
    assert(n == 1);
    int key = get_int(args[0]);
    return IsKeyPressed(key) ? t : nil;
}

static emacs_value
rl_draw_fps(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr) {
    int x = get_int(args[0]);
    int y = get_int(args[1]);

    exit_check();

    DrawFPS(x, y);
    return nil;
}

static emacs_value
rl_draw_text(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr) {
    assert(n == 5);

    string text = get_string(args[0]);
    int x = get_int(args[1]);
    int y = get_int(args[2]);
    int font_size = get_int(args[3]);
    Color color = get_color(args[4]);

    DrawText(text.text, x, y, font_size, color);

    free_string(&text);
    return nil;
}

static emacs_value
rl_draw_grid(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr) {
    assert(n == 2);

    int slices = get_int(args[0]);
    float spacing = get_float(args[1]);

    DrawGrid(slices, spacing);

    return nil;
}

static emacs_value
rl_fade(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr) {
    assert(n == 2);

    Color color = get_color(args[0]);
    float alpha = get_float(args[1]);

    return new_color(Fade(color, alpha));
}

static emacs_value
rl_get_random_value(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr) {
    assert(n == 2);

    int min = get_int(args[0]);
    int max = get_int(args[1]);

    return new_float(GetRandomValue(min, max));
}


static emacs_value
rl_get_time(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr) {
    return new_float(GetTime());
}



static emacs_value
rl_push_matrix(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr) {
    assert(n == 0);
    rlPushMatrix();
    return nil;
}

static emacs_value
rl_pop_matrix(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr) {
    assert(n == 0);
    rlPopMatrix();
    return nil;
}

static emacs_value
rl_translatef(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr) {
    assert(n == 3);

    float x = get_float(args[0]);
    float y = get_float(args[1]);
    float z = get_float(args[2]);
    rlTranslatef(x, y, z);

    return nil;
}

static emacs_value
rl_rotatef(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr) {
    assert(n == 4);

    float angle = get_float(args[0]);
    float x = get_float(args[1]);
    float y = get_float(args[2]);
    float z = get_float(args[3]);
    rlRotatef(angle, x, y, z);

    return nil;
}



int
emacs_module_init (struct emacs_runtime *runtime) {
    printf("raylib was built againts %d\n", EMACS_MAJOR_VERSION);

    if (runtime->size < sizeof(*runtime)) {
        printf("Dynamic size is smaller than static size.\n");
        return 1;
    }

    emacs_env *env = runtime->get_environment(runtime);

    // TODO: figure out what emacs version do we really need.
    int emacs_version = 0;
    if (env->size >= sizeof (struct emacs_env_30))
        emacs_version = 30;
    else if (env->size >= sizeof (struct emacs_env_25))
        emacs_version = 25;
    else {
        printf("Unknown or unsupported version.\n");
        return 2;
    }

    printf("raylib: Using Emacs env version: %d\n", emacs_version);

    emacs_value defalias = S("defalias");

#define make_function(func, arity, name, docstring)                     \
    env->funcall(env, defalias, 2, (emacs_value[]) {                        \
            env->intern(env, name),                                     \
            env->make_function(env, arity, arity, func, docstring, 0),  \
        })

    make_function(rl_init_window, 3, "rl-init-window", "TODO");
    make_function(rl_close_window, 0, "rl-close-window", "TODO");
    make_function(rl_window_should_close, 0, "rl-window-should-close", "TODO");

    make_function(rl_get_frame_time, 0, "rl-get-frame-time", "TODO");



    make_function(rl_begin_drawing, 0, "rl-begin-drawing", "TODO");
    make_function(rl_end_drawing, 0, "rl-end-drawing", "TODO");

    make_function(rl_begin_mode_2d, 1, "rl-begin-mode-2d", "TODO");
    make_function(rl_end_mode_2d, 0, "rl-end-mode-2d", "TODO");

    make_function(rl_clear_background, 1, "rl-clear-background", "TODO");

    make_function(rl_draw_circle, 4, "rl-draw-circle", "TODO");
    make_function(rl_draw_circle_v, 3, "rl-draw-circle-v", "TODO");

    make_function(rl_draw_line, 5, "rl-draw-line", "TODO");
    make_function(rl_draw_line_v, 3, "rl-draw-line-v", "TODO");

    make_function(rl_draw_rectangle, 5, "rl-draw-rectangle", "TODO");
    make_function(rl_draw_rectangle_rec, 2, "rl-draw-rectangle-rec", "TODO");
    make_function(rl_draw_rectangle_lines, 5, "rl-draw-rectangle-lines", "TODO");

    make_function(rl_is_mouse_button_pressed, 1, "rl-is-mouse-button-pressed", "TODO");
    make_function(rl_is_mouse_button_down, 1, "rl-is-mouse-button-down", "TODO");
    make_function(rl_is_mouse_button_released, 1, "rl-is-mouse-button-released", "TODO");
    make_function(rl_is_mouse_button_up, 1, "rl-is-mouse-button-up", "TODO");

    make_function(rl_get_mouse_position, 0, "rl-get-mouse-position", "TODO");
    make_function(rl_get_mouse_delta, 0, "rl-get-mouse-delta", "TODO");
    make_function(rl_get_mouse_wheel_move, 0, "rl-get-mouse-wheel-move", "TODO");

    make_function(rl_get_screen_to_world_2d, 2, "rl-get-screen-to-world-2d", "TODO");

    make_function(rl_is_key_down, 1, "rl-is-key-down", "TODO");
    make_function(rl_is_key_up, 1, "rl-is-key-up", "TODO");
    make_function(rl_is_key_pressed, 1, "rl-is-key-pressed", "TODO");

    make_function(rl_draw_fps, 2, "rl-draw-fps", "TODO");
    make_function(rl_draw_text, 5, "rl-draw-text", "TODO");

    make_function(rl_draw_grid, 2, "rl-draw-grid", "TODO");

    make_function(rl_fade, 2, "rl-fade", "TODO");

    make_function(rl_get_random_value, 2, "rl-get-random-value", "TODO");

    make_function(rl_get_time, 0, "rl-get-time", "TODO");


    // NOTE: rlgl.h stuff
    make_function(rl_push_matrix, 0, "rl-push-matrix", "TODO");
    make_function(rl_pop_matrix, 0, "rl-pop-matrix", "TODO");
    make_function(rl_translatef, 3, "rl-translatef", "TODO");
    make_function(rl_rotatef, 4, "rl-rotatef", "TODO");

    // TODO: use https://www.gnu.org/software/emacs/manual/html_node/elisp/Function-Documentation.html
    // to announce function arguments.

    emacs_value raylib = env->intern(env, "raylib");
    env->funcall(env, S("provide"), 1, &raylib);

    message("raylib is ready!");

    return 0;
}
