:- module(plx, [
    x_open_display/2,
    default_root_window/2,
    default_screen/2,
    x_set_error_handler/1,
    xrr_query_extension/3,
    xrr_select_input/3,
    xrr_get_screen_resources/4
]).

:- use_module(library(ffi)).

:- initialization(use_foreign_module("libX11.so", [
    'XOpenDisplay'([cstr], ptr),
    'XCloseDisplay'([ptr], void),

    'XSetErrorHandler'([ptr], void),

    'XSetCloseDownMode'([ptr, i32], void),

    'XCreateFontCursor'([ptr, i32], u64),

    'XDefineCursor'([ptr, u64, u64], void),
    'XFreeCursor'([ptr, u64], void),

    'XGrabKey'([ptr, i32, u32, u64, bool, i32, i32], void),
    'XGrabButton'([ptr, u32, u32, u64, bool, u32, i32, i32, u64, u64], void),
    'XGrabPointer'([ptr, u64, bool, u32, i32, i32, u64, u64, u64], void),
    'XGrabServer'([ptr], void),

    'XUngrabKey'([ptr, i32, u32, u64], void),
    'XUngrabButton'([ptr, u32, u32, u64], void),
    'XUngrabPointer'([ptr, u64], void),
    'XUngrabServer'([ptr], void),

    'XKeysymToKeycode'([ptr, u64], i32),
    'XStringToKeysym'([cstr], u64),

    'XNextEvent'([ptr, ptr], void),
    'XSendEvent'([ptr, u64, bool, u64, ptr], void),

    'XRaiseWindow'([ptr, u64], void),
    'XGetWindowAttributes'([ptr, u64, ptr], i32),
    'XChangeWindowAttributes'([ptr, u64, u64, ptr], void),
    'XMoveResizeWindow'([ptr, u64, i32, i32, u32, u32], void),

    'XSelectInput'([ptr, u64, i64], void),

    'XMapWindow'([ptr, u64], void),
    'XConfigureWindow'([ptr, u64, u32, ptr], void),
    'XSetWindowBorder'([ptr, u64, u64], void),

    'XSetInputFocus'([ptr, u64, i32, u64], void),

    'XKillClient'([ptr, u64], void),

    'XSync'([ptr, bool], void),

    'XInternAtom'([ptr, cstr, bool], u64),
    'XGetClassHint'([ptr, u64, ptr], bool)
])).

%% Macros
%    'DefaultRootWindow'([ptr], u64),
%    'DefaultScreen'([ptr], i32),
%    'DefaultVisual'([ptr, i32], ptr),
%    'DefaultColormap'([ptr, i32], u64)
%

:- initialization(use_foreign_module("libXft.so", [
    'XftColorAllocName'([ptr, ptr, u64, cstr, ptr], bool)
])).

:- initialization(use_foreign_module("libXrandr.so", [
    'XRRQueryExtension'([ptr, ptr, ptr], bool),

    'XRRSelectInput'([ptr, u64, i32], void),

    'XRRGetScreenResources'([ptr, u64], ptr),
    'XRRFreeScreenResources'([ptr], void),

    'XRRGetOutputInfo'([ptr, ptr, u64], ptr),
    'XRRFreeOutputInfo'([ptr], void),

    'XRRGetCrtcInfo'([ptr, ptr, u64], ptr),
    'XRRFreeCrtcInfo'([ptr], void)
])).



x_open_display(DpName, DpPtr) :-
    % TODO how to pass null for DpName?
    ffi:'XOpenDisplay'(DpName, DpPtr).

default_root_window(DpPtr, Win) :-
    ffi:'DefaultRootWindow'(DpPtr, Win).

default_screen(DpPtr, Screen) :-
    ffi:'DefaultScreen'(DpPtr, Screen).

x_set_error_handler(_).

xrr_query_extension(_,_,_).

xrr_select_input(_,_,_).

xrr_get_screen_resources(_,_,_,_).

xrr_get_output_info(_,_,_,_).

xrr_get_crtc_info(_,_,_,_).
