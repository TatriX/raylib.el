#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <raylib.h>

#include <emacs-module.h>

int plugin_is_GPL_compatible;

bool has_window;

#define S(s) (env->intern(env, s))

// NOTE: emacs values *must* not be stored between calls.
#define nil S("nil")
#define t S("t")

// TODO: figure out when this should be used actually
#define check() if (env->non_local_exit_check(env) != emacs_funcall_exit_return) return nil;

#define get_float(value) env->extract_float(env, (value))
#define get_int(value) env->extract_integer(env, (value))
#define get_string(value) extract_string(env, (value))
#define aref(vec, index) env->vec_get(env, (vec), (index))

#define get_color(value) extract_color(env, (value))

// NOTE: signal() must be called with string literals!
#define signal(error_symbol, error_string) call_signal(env, error_symbol, error_string, sizeof(error_string) - 1)

// NOTE: message() must be called with string literals!
#define message(string) call_message(env, string, sizeof(string) - 1)

static inline Color
extract_color(emacs_env *env, emacs_value vec) {
    int size = env->vec_size(env, vec);
    assert(size == 4);
    Color color = {
        get_int(aref(vec, 0)),
        get_int(aref(vec, 1)),
        get_int(aref(vec, 2)),
        get_int(aref(vec, 3)),
    };
    return color;
}

static void
call_message(emacs_env *env, const char *text, int text_len) {
    emacs_value message = env->intern(env, "message");
    emacs_value string = env->make_string(env, text, text_len);
    env->funcall(env, message, 1, &string);
}

static void
call_signal(emacs_env *env, const char *error_symbol, const char *error_string, int error_string_len) {
    emacs_value signal = env->intern(env, error_symbol);
    emacs_value message = env->make_string(env, error_string, error_string_len);
    env->non_local_exit_signal(env, signal, message);
}


// NOTE: This should be equal to MAX_TEXT_BUFFER_LENGTH
char global_text_buffer[1024];

typedef struct string {
    char *text;
    ptrdiff_t size;
} string;

static inline string
extract_string(emacs_env *env, emacs_value value) {
    struct string result;
    env->copy_string_contents(env, value, NULL, &result.size);

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
rl_clear_background(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr) {
    assert(n == 1);
    Color color = extract_color(env, args[0]);
    ClearBackground(color);
    return nil;
}

static emacs_value
rl_draw_circle(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr) {
    assert(n == 4);

    float x = get_float(args[0]);
    float y = get_float(args[1]);
    float radius = get_float(args[2]);
    Color color = get_color(args[3]);

    DrawCircle(x, y, radius, color);

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
rl_get_time(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr) {
    return env->make_float(env, GetTime());
}



int
emacs_module_init (struct emacs_runtime *runtime) {
    printf("raylib was built againts %d\n", EMACS_MAJOR_VERSION);

    if (runtime->size < sizeof(*runtime)) {
        return 1;
    }

    emacs_env *env = runtime->get_environment(runtime);

    // TODO: figure out what emacs version do we really need.
    int emacs_version = 0;
    if (env->size >= sizeof (struct emacs_env_30))
        emacs_version = 30;
    else if (env->size >= sizeof (struct emacs_env_25))
        emacs_version = 25;
    else
        return 2; /* Unknown or unsupported version.  */

    printf("raylib: Using Emacs env version: %d\n", emacs_version);

    emacs_value fset = env->intern(env, "fset");

#define make_function(func, arity, name, docstring)                     \
    env->funcall(env, fset, 2, (emacs_value[]) {                        \
            env->intern(env, name),                                     \
            env->make_function(env, arity, arity, func, docstring, 0),  \
        })

    make_function(rl_init_window, 3, "rl-init-window", "TODO");
    make_function(rl_close_window, 0, "rl-close-window", "TODO");
    make_function(rl_window_should_close, 0, "rl-window-should-close", "TODO");

    make_function(rl_begin_drawing, 0, "rl-begin-drawing", "TODO");
    make_function(rl_end_drawing, 0, "rl-end-drawing", "TODO");

    make_function(rl_clear_background, 1, "rl-clear-background", "TODO");

    make_function(rl_draw_circle, 4, "rl-draw-circle", "TODO");

    make_function(rl_draw_text, 5, "rl-draw-text", "TODO");

    make_function(rl_get_time, 0, "rl-get-time", "TODO");

    emacs_value raylib = env->intern(env, "raylib");
    env->funcall(env, S("provide"), 1, &raylib);

    message("raylib is ready!");

    return 0;
}
