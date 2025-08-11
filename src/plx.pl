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
:- use_module(library(lists)).
:- use_module(library(format)).
:- use_module(library(error)).
:- use_module(library(si)).
:- use_module(library(charsio)).

% Note: assumes
%    int            = i32
%    unsigned int   = u32
%    long           = i64
%    unsigned long  = u64


% define X11 structs
:- initialization(foreign_struct('XRenderColor', [
    u16, u16, u16, u16
])).

:- initialization(foreign_struct('XSetWindowAttributes', [
    u64, u64, u64, u64, i32, i32, i32, u64, u64, i32, i64, i64, i32, u64, u64
])).

:- initialization(foreign_struct('XTextProperty', [
    u8, u64, i32, u64
])).

% define Xft structs
:- initialization(foreign_struct('XftColor', [
    u64, 'XRenderColor'
])).

% define Xrandr structs
:- initialization(foreign_struct('XRRCrtcInfo', [
    u64, i32, i32, u32, u32, u64, u16, i32, ptr, u16, i32, ptr
])).

:- initialization(foreign_struct('XRRScreenResources', [
    u64, u64, i32, ptr, i32, ptr, i32, ptr
])).

:- initialization(foreign_struct('XRROutputInfo', [
    u64, u64, cstr, i32, u64, u64, u16, u16, i32, ptr, i32, ptr, i32, i32, ptr
])).

% bind to x11plwm wrapper shared library
:- initialization(use_foreign_module("bin/x11plwm.so", [
    'x11plwm_DefaultRootWindow'([ptr], u64),
    'x11plwm_DefaultScreen'([ptr], i32),
    'x11plwm_DefaultColormap'([ptr, i32], u64),
    'x11plwm_DefaultVisual'([ptr, i32], ptr),
    'x11plwm_set_error_handler'([i32], void)
])).

% bind to X11 shared library
:- initialization(use_foreign_module("libX11.so", [
    'XOpenDisplay'([cstr], ptr),
    'XCloseDisplay'([ptr], i32),

    'XSetCloseDownMode'([ptr, i32], i32),

    'XCreateFontCursor'([ptr, i32], u64),

    'XDefineCursor'([ptr, u64, u64], i32),
    'XFreeCursor'([ptr, u64], i32),

    'XGrabKey'([ptr, i32, u32, u64, i32, i32, i32], i32),
    'XGrabButton'([ptr, u32, u32, u64, i32, u32, i32, i32, u64, u64], i32),
    'XGrabPointer'([ptr, u64, i32, u32, i32, i32, u64, u64, u64], i32),
    'XGrabServer'([ptr], i32),

    'XUngrabKey'([ptr, i32, u32, u64], i32),
    'XUngrabButton'([ptr, u32, u32, u64], i32),
    'XUngrabPointer'([ptr, u64], i32),
    'XUngrabServer'([ptr], i32),

    'XKeysymToKeycode'([ptr, u64], i32),
    'XStringToKeysym'([cstr], u64),

    'XNextEvent'([ptr, ptr], i32),
    'XSendEvent'([ptr, u64, i32, u64, ptr], i32),

    'XRaiseWindow'([ptr, u64], void),
    'XGetWindowAttributes'([ptr, u64, ptr], i32),
    'XChangeWindowAttributes'([ptr, u64, u64, ptr], i32),
    'XMoveResizeWindow'([ptr, u64, i32, i32, u32, u32], i32),

    'XSelectInput'([ptr, u64, i64], i32),

    'XMapWindow'([ptr, u64], i32),
    'XConfigureWindow'([ptr, u64, u32, ptr], i32),
    'XSetWindowBorder'([ptr, u64, u64], i32),

    'XSetInputFocus'([ptr, u64, i32, u64], i32),

    'XKillClient'([ptr, u64], i32),

    'XSync'([ptr, i32], i32),

    'XInternAtom'([ptr, cstr, i32], u64),
    'XGetClassHint'([ptr, u64, ptr], i32),

    'XInternAtom'([ptr, cstr, i32], u64),

    'XChangeProperty'([ptr, u64, u64, u64, i32, i32, ptr, i32], i32),

    'XCreateSimpleWindow'([ptr, u64, i32, i32, u32, u32, u32, u64, u64], u64)
])).

% bind to Xft shared library
:- initialization(use_foreign_module("libXft.so", [
    'XftColorAllocName'([ptr, ptr, u64, cstr, ptr], i32)
])).


% bind to Xrandr shared library
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


bool_int(Bool, Int) :-

    (   nonvar(Bool) -> (
            Bool = true -> Int = 1
        ;   Bool = false -> Int = 0
        ;   domain_error(boolean_atom, Bool, bool_int/2)
        )
    ;   nonvar(Int) -> (
            Int = 1 -> Bool = true
        ;   Int = 0 -> Bool = false
        ;   domain_error(boolean_int, Int, bool_int/2)
        )
    ;   instantiation_error([Bool, Int], bool_int/2)
    )
.


x_open_display(DpName, DpPtr) :-
    ( DpName = "" -> DpArg = 0
    ; DpArg = DpName
    ),
    ffi:'XOpenDisplay'(DpArg, DpPtr).

default_root_window(DpPtr, Win) :-
    ffi:'x11plwm_DefaultRootWindow'(DpPtr, Win).

default_screen(DpPtr, Screen) :-
    ffi:'x11plwm_DefaultScreen'(DpPtr, Screen).

default_colormap(DpPtr, Screen, Cm) :-
    ffi:'x11plwm_DefaultColormap'(DpPtr, Screen, Colormap),
    Colormap = Cm.

default_visual(DpPtr, Screen, VisPtr) :-
    ffi:'x11plwm_DefaultVisual'(DpPtr, Screen, Ptr),
    VisPtr = Ptr.

x_set_error_handler(false) :- ffi:'x11plwm_set_error_handler'(0).
x_set_error_handler(true) :- ffi:'x11plwm_set_error_handler'(1).

x_intern_atom(Dp, Name, IfExists, Atom) :-
    bool_int(IfExists, If),
    ffi:'XInternAtom'(Dp, Name, If, A),
    Atom = A.

x_change_property(Dp, Win, Prop, Atom, Format, Mode, Data, NElements) :-
    (chars_si(Data) ->
        % special treatment for strings
        chars_utf8bytes(Data, Codes),
        ElemType = u8,
        ArrayValues = Codes
    ;   ElemType = u64,
        ArrayValues = Data
    ),
    length(ArrayValues, Len),
    ( Len = NElements -> true
    ; throw(error(assert(Len = NElements), x_change_property/8))
    ),
    ffi:array_type(ElemType, Len, ArrayType),
    ffi:with_locals([
        let(ArrayPtr, ArrayType, [ArrayType | ArrayValues])
    ],
        ffi:'XChangeProperty'(Dp, Win, Prop, Atom, Format, Mode, ArrayPtr, Len, _)
    ).

x_change_window_attributes(Dp, Win, ValueMask, EventMask) :-
    ffi:with_locals([
        let(WinAttributesPtr, 'XSetWindowAttributes', ['XSetWindowAttributes', 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, EventMask, 0, 0, 0, 0])
    ],
        ffi:'XChangeWindowAttributes'(Dp, Win, ValueMask, WinAttributesPtr, _)
    ).

x_create_simple_window(Dp, Parent, X, Y, Width, Height, BorderW, Border, Background, Win) :-
    ffi:'XCreateSimpleWindow'(Dp, Parent, X, Y, Width, Height, BorderW, Border, Background, W),
    Win = W.

x_select_input(Dp, Win, EventMask) :-
    ffi:'XSelectInput'(Dp, Win, EventMask, _).

x_create_font_cursor(Dp, Shape, Cursor) :-
    ffi:'XCreateFontCursor'(Dp, Shape, C),
    C = Cursor.

x_define_cursor(Dp, Win, Cursor) :-
    ffi:'XDefineCursor'(Dp, Win, Cursor, _).

x_grab_key(Dp, KeyCode, Modifiers, GrabWindow, OwnerEvents, PointerMode, KeyboardMode) :-
    bool_int(OwnerEvents, Oes),
    ffi:'XGrabKey'(Dp, KeyCode, Modifiers, GrabWindow, Oes, PointerMode, KeyboardMode, _).

x_ungrab_key(Dp, KeyCode, Modifiers, GrabWindow) :-
    ffi:'XUngrabKey'(Dp, KeyCode, Modifiers, GrabWindow, _).

x_grab_button(Dp, Button, Modifiersm, GrabWindow, OwnerEvents, EventMask, PointerMode, KeyboardMode, ConfineTo, Cursor) :-
    bool_int(OwnerEvents, Oes),
    ffi:'XGrabButton'(Dp, Button, Modifiersm, GrabWindow, Oes, EventMask, PointerMode, KeyboardMode, ConfineTo, Cursor, _).

x_ungrab_button(Dp, Button, Modifiers, GrabWindow) :-
    ffi:'XUngrabButton'(Dp, Button, Modifiers, GrabWindow, _).

x_string_to_keysym(KeyName, KeySymbol) :-
    ffi:'XStringToKeysym'(KeyName, KeySymbol).

x_keysym_to_keycode(Dp, KeySymbol, KeyCode) :-
    ffi:'XKeysymToKeycode'(Dp, KeySymbol, KeyCode).

gen_local_str(Atom, let(_Ptr, ArrayType, [ArrayType | Utf8Nul ])) :-
    atom_chars(Atom, String),
    chars_utf8bytes(String, Utf8),
    append(Utf8, [0], Utf8Nul),
    length(Utf8Nul, Len),
    ffi:array_type(u8, Len, ArrayType).


x_utf8_text_list_to_text_property(Dp, List, Count, Style, TextPropReturn) :- call_with_error_context(x_utf8_text_list_to_text_property_(Dp, List, Count, Style, TextPropReturn), predicate-x_utf8_text_list_to_text_property/5).

x_utf8_text_list_to_text_property_(Dp, List, Count, Style, TextPropReturn) :-
    length(List, Len),
    ( Len = Count -> true
    ; throw(error(assert(Len = Count), x_utf8_text_list_to_text_property/5))
    ),
    maplist(
        gen_local_str,
        List,
        Locals
    ),
    maplist(arg(1), Locals, Ptrs),
    format("AfterMapList~n", []),
    ffi:array_type(ptr, Count, PointerList),
    ffi:with_locals(
        Locals,
        ffi:with_locals([
            let(Strs, PointerList, [PointerList | Ptrs])
        ],
            ffi:allocate(c, 'XTextProperty', ['XTextProperty' , 0, 0, 0, 0], TProp),
            ffi:'Xutf8TextListToTextProperty'(Dp, Strs, Style, TProp, _)
        )
    ),
    TProp = TextPropReturn.


xrr_query_extension(Dp, Event, Error) :-
    ffi:with_locals([
        let(EventPtr, i32, 0),
        let(ErrorPtr, i32, 0)
    ],
    (
        ( ffi:'XRRQueryExtension'(Dp, EventPtr, ErrorPtr) -> true
        ; writeln("XRRQueryExtension() failed!"), false
        ),
        ffi:read_ptr(i32, EventPtr, FEvent),
        ffi:read_ptr(i32, ErrorPtr, FError),
        Event = FEvent,
        Error = FError
    )).

xrr_select_input(Dp, Win, Mask) :- ffi:'XRRSelectInput'(Dp, Win, Mask).

xrr_get_screen_resources(Dp, Win, ScreenResources, Out) :-
    ffi:'XRRGetScreenResources'(Dp, Win, SR),
    (
        SR = 0 -> writeln("xrr_get_screen_resources: XRRGetScreenResources() returned NULL!"), false
        ; true
    ),
    ( ScreenResources = SR -> true
    ; ffi:'XRRFreeScreenResources'(SR), false
    ),
    ffi:read_ptr('XRRScreenResources', SR, Val),
    ['XRRScreenResources', _timestamp, _configTimestamp, _ncrtc, _crtcs, Noutput, Outputs | _] = Val,
    build_outputs(Noutput, Outputs, Outs),
    ( Out = Outs -> true
    ; ffi:'XRRFreeScreenResources'(SR), false
    ).

build_outputs(Counter, Ptr, Out) :-
    (Counter = 0 -> Out = []
    ; Out = [O|Os],
      ffi:read_ptr(u64, Ptr, O),
      Rem is Counter - 1,
      Next is Ptr + 8,
      build_outputs(Rem, Next, Os)
    ).

xrr_get_output_info(Dp, ScreenResources, OutIdx,OutInfo) :-
    ffi:read_ptr('XRRScreenResources', ScreenResources, Val),
    ['XRRScreenResources', _timestamp, _configTimestamp, _ncrtc, _crtcs, Noutput, Outputs | _] = Val,
    (
        (OutIdx < 0 ; Noutput =< OutIdx) -> format("xrr_get_output_info: output_index: ~d is out of bounds: 0..~d!~n", [OutIdx, Noutput]), false
        ; true
    ),
    EntryPtr is Outputs + (8 * OutIdx),
    ffi:read_ptr(u64, EntryPtr, Output),
    ffi:'XRRGetOutputInfo'(Dp, ScreenResources, Output, OutputPtr),
    (
        OutputPtr = 0 -> writeln("xrr_get_output_info: XRRGetOutputInfo() returned NULL!"), false
        ; true
    ),
    ffi:read_ptr('XRROutputInfo', OutputPtr, OutputStruct),
    ffi:'XRRFreeOutputInfo'(OutputPtr),
    ['XRROutputInfo', _timestamp, Crtc, AtomName, _nameLen, _width, _height, Connection | _] = OutputStruct,
    atom_chars(AtomName, Name),
    OutInfo = [Name, Connection, Crtc].

xrr_free_screen_resources(Sr) :- ffi:'XRRFreeScreenResources'(Sr).

xft_color_alloc_name(Dp, Vis, ColorMap, Name, Res) :-
    ffi:with_locals([
        let(ResPtr, 'XftColor', ['XftColor', 0, ['XRenderColor', 0,0,0,0]])
    ],
    (
        ffi:'XftColorAllocName'(Dp, Vis, ColorMap, Name, ResPtr, _),
        ffi:read_ptr('XftColor', ResPtr, ResStruct),
        ['XftColor', Res, _] = ResStruct
    )).

xrr_get_crtc_info(Dp, ScreenResources, Crtc, CrtcInfo) :-
    ffi:'XRRGetCrtcInfo'(Dp, ScreenResources, Crtc, CrtcInfoPtr),
    ( CrtcInfoPtr = 0 -> writeln("xrr_get_crtc_info: XRRGetCrtcInfo() returned NULL!"), false; true),
    ffi:read_ptr('XRRCrtcInfo', CrtcInfoPtr, Info),
    ffi:'XRRFreeCrtcInfo'(CrtcInfoPtr),
    ['XRRCrtcInfo', _timestamp, X, Y, W, H | _ ] = Info,
    CrtcInfo = [X, Y, W, H].
