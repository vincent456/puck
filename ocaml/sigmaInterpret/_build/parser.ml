exception Error

type token = 
  | STAR
  | SIGMA
  | RPAREN
  | RBRACKET
  | PLUS
  | LPAREN
  | LET
  | LBRACKET
  | LARROW
  | LAMBDA
  | IN
  | IDENT of (
# 17 "parser.mly"
       (string)
# 19 "parser.ml"
)
  | EQUAL
  | EOF
  | DOT
  | CONST_REAL of (
# 13 "parser.mly"
       (float)
# 27 "parser.ml"
)
  | CONST_INT of (
# 12 "parser.mly"
       (int)
# 32 "parser.ml"
)
  | CONST_BOOL of (
# 11 "parser.mly"
       (bool)
# 37 "parser.ml"
)
  | COMMA
  | COLON
  | AND

and _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  mutable _menhir_token: token;
  mutable _menhir_startp: Lexing.position;
  mutable _menhir_endp: Lexing.position;
  mutable _menhir_shifted: int
}

and _menhir_state = 
  | MenhirState57
  | MenhirState52
  | MenhirState44
  | MenhirState34
  | MenhirState32
  | MenhirState29
  | MenhirState25
  | MenhirState20
  | MenhirState15
  | MenhirState11
  | MenhirState7
  | MenhirState5
  | MenhirState4
  | MenhirState3
  | MenhirState1


# 1 "parser.mly"
  
  open Parsed_terms
  open Parsed_terms_printer
  (* let loc () = symbol_start_pos (), symbol_end_pos () *)
  (* let mk_expr e = { pexpr_desc = e; pexpr_loc = loc () } *)


# 78 "parser.ml"
let _eRR =
  Error

let rec _menhir_goto_def_list : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_def_list -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv321) * _menhir_state * 'tv_def_list) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv319) * _menhir_state * 'tv_def_list) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv315) * _menhir_state * 'tv_def_list) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv313) * _menhir_state * 'tv_def_list) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, ds) = _menhir_stack in
            let _v : 'tv_defs = 
# 57 "parser.mly"
                       ( ds )
# 105 "parser.ml"
             in
            _menhir_goto_defs _menhir_env _menhir_stack _v) : 'freshtv314)) : 'freshtv316)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv317) * _menhir_state * 'tv_def_list) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv318)) : 'freshtv320)) : 'freshtv322)
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv325 * _menhir_state * 'tv_def) * _menhir_state * 'tv_def_list) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv323 * _menhir_state * 'tv_def) * _menhir_state * 'tv_def_list) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, d), _, ds) = _menhir_stack in
        let _v : 'tv_def_list = 
# 62 "parser.mly"
                            ( d :: ds )
# 124 "parser.ml"
         in
        _menhir_goto_def_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv324)) : 'freshtv326)
    | _ ->
        _menhir_fail ()

and _menhir_goto_method_decl : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_method_decl -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv311 * _menhir_state * 'tv_method_decl) = Obj.magic _menhir_stack in
    ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv309 * _menhir_state * 'tv_method_decl) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | COMMA ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv303 * _menhir_state * 'tv_method_decl) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv301 * _menhir_state * 'tv_method_decl) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | IDENT _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
        | RBRACKET ->
            _menhir_reduce15 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44) : 'freshtv302)) : 'freshtv304)
    | RBRACKET ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv305 * _menhir_state * 'tv_method_decl) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, m) = _menhir_stack in
        let _v : 'tv_method_list = 
# 125 "parser.mly"
 ( [m] )
# 164 "parser.ml"
         in
        _menhir_goto_method_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv306)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv307 * _menhir_state * 'tv_method_decl) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv308)) : 'freshtv310)) : 'freshtv312)

and _menhir_goto_update : _menhir_env -> 'ttv_tail -> 'tv_update -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv299 * _menhir_state * 'tv_term) * (
# 17 "parser.mly"
       (string)
# 181 "parser.ml"
    )) = Obj.magic _menhir_stack in
    let (_v : 'tv_update) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv297 * _menhir_state * 'tv_term) * (
# 17 "parser.mly"
       (string)
# 188 "parser.ml"
    )) = Obj.magic _menhir_stack in
    let (m : 'tv_update) = _v in
    ((let ((_menhir_stack, _menhir_s, t), label) = _menhir_stack in
    let _v : 'tv_term = 
# 102 "parser.mly"
        ( PUpdate(t, label, m) )
# 195 "parser.ml"
     in
    _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v) : 'freshtv298)) : 'freshtv300)

and _menhir_goto_mathExpr : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_mathExpr -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv295) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_mathExpr) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv293) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (e : 'tv_mathExpr) = _v in
    ((let _v : 'tv_term = 
# 107 "parser.mly"
 ( e )
# 212 "parser.ml"
     in
    _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v) : 'freshtv294)) : 'freshtv296)

and _menhir_run25 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_term -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv291 * _menhir_state * 'tv_term) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | CONST_BOOL _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | CONST_INT _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | CONST_REAL _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | IDENT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | LAMBDA ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | LBRACKET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | LPAREN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25) : 'freshtv292)

and _menhir_run34 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_term -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv289 * _menhir_state * 'tv_term) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | CONST_BOOL _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | CONST_INT _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | CONST_REAL _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | IDENT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | LAMBDA ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | LBRACKET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | LPAREN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34) : 'freshtv290)

and _menhir_run27 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_term -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv287 * _menhir_state * 'tv_term) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | IDENT _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv283 * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
        let (_v : (
# 17 "parser.mly"
       (string)
# 281 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv281 * _menhir_state * 'tv_term) * (
# 17 "parser.mly"
       (string)
# 289 "parser.ml"
        )) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv271) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv269) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | EQUAL ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv265) = Obj.magic _menhir_stack in
                ((let _tok = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv263) = _menhir_stack in
                let (_tok : token) = _tok in
                ((match _tok with
                | CONST_BOOL _v ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
                | CONST_INT _v ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
                | CONST_REAL _v ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
                | IDENT _v ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
                | LAMBDA ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState32
                | LBRACKET ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState32
                | LPAREN ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState32
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32) : 'freshtv264)) : 'freshtv266)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv267) = Obj.magic _menhir_stack in
                (raise _eRR : 'freshtv268)) : 'freshtv270)) : 'freshtv272)
        | LARROW ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv275) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv273) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | SIGMA ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29) : 'freshtv274)) : 'freshtv276)
        | AND | COMMA | CONST_BOOL _ | CONST_INT _ | CONST_REAL _ | DOT | EOF | IDENT _ | IN | LBRACKET | LPAREN | PLUS | RBRACKET | RPAREN | STAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv277 * _menhir_state * 'tv_term) * (
# 17 "parser.mly"
       (string)
# 352 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, t), label) = _menhir_stack in
            let _v : 'tv_term1 = 
# 91 "parser.mly"
 ( PSelection (t, label) )
# 358 "parser.ml"
             in
            _menhir_goto_term1 _menhir_env _menhir_stack _menhir_s _v) : 'freshtv278)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv279 * _menhir_state * 'tv_term) * (
# 17 "parser.mly"
       (string)
# 368 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv280)) : 'freshtv282)) : 'freshtv284)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv285 * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv286)) : 'freshtv288)

and _menhir_goto_term : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_term -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState15 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv171 * _menhir_state) * (
# 17 "parser.mly"
       (string)
# 389 "parser.ml"
        )) * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv169 * _menhir_state) * (
# 17 "parser.mly"
       (string)
# 397 "parser.ml"
        )) * _menhir_state * 'tv_term) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DOT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EOF | IN | RBRACKET | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv165 * _menhir_state) * (
# 17 "parser.mly"
       (string)
# 412 "parser.ml"
            )) * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), b), _, t) = _menhir_stack in
            let _v : 'tv_term = 
# 105 "parser.mly"
 ( PLambda (b, t))
# 418 "parser.ml"
             in
            _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v) : 'freshtv166)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv167 * _menhir_state) * (
# 17 "parser.mly"
       (string)
# 428 "parser.ml"
            )) * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv168)) : 'freshtv170)) : 'freshtv172)
    | MenhirState25 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv179 * _menhir_state * 'tv_term) * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv177 * _menhir_state * 'tv_term) * _menhir_state * 'tv_term) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DOT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EOF | IN | PLUS | RBRACKET | RPAREN | STAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv173 * _menhir_state * 'tv_term) * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, t1), _, t2) = _menhir_stack in
            let _v : 'tv_mathExpr = 
# 112 "parser.mly"
                           ( PMathExpr (Times, t1, t2) )
# 450 "parser.ml"
             in
            _menhir_goto_mathExpr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv174)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv175 * _menhir_state * 'tv_term) * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv176)) : 'freshtv178)) : 'freshtv180)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv187) * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv185) * _menhir_state * 'tv_term) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DOT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EOF | IN | RBRACKET | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv181) * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, t) = _menhir_stack in
            let _v : 'tv_update = 
# 119 "parser.mly"
        ( None, t )
# 482 "parser.ml"
             in
            _menhir_goto_update _menhir_env _menhir_stack _v) : 'freshtv182)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv183) * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv184)) : 'freshtv186)) : 'freshtv188)
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv195 * _menhir_state * 'tv_term) * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv193 * _menhir_state * 'tv_term) * _menhir_state * 'tv_term) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DOT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EOF | IN | PLUS | RBRACKET | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv189 * _menhir_state * 'tv_term) * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, t1), _, t2) = _menhir_stack in
            let _v : 'tv_mathExpr = 
# 111 "parser.mly"
                           ( PMathExpr (Plus, t1, t2) )
# 512 "parser.ml"
             in
            _menhir_goto_mathExpr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv190)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv191 * _menhir_state * 'tv_term) * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv192)) : 'freshtv194)) : 'freshtv196)
    | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv213 * _menhir_state) * (
# 17 "parser.mly"
       (string)
# 527 "parser.ml"
        )) * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv211 * _menhir_state) * (
# 17 "parser.mly"
       (string)
# 535 "parser.ml"
        )) * _menhir_state * 'tv_term) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DOT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EOF | IN | RBRACKET | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv207 * _menhir_state) * (
# 17 "parser.mly"
       (string)
# 550 "parser.ml"
            )) * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), b), _, t) = _menhir_stack in
            let _v : 'tv_method_def = 
# 138 "parser.mly"
 (  Some b, t )
# 556 "parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv205) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_method_def) = _v in
            ((match _menhir_s with
            | MenhirState29 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv199) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_method_def) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv197) = Obj.magic _menhir_stack in
                let (_ : _menhir_state) = _menhir_s in
                let (m : 'tv_method_def) = _v in
                ((let _v : 'tv_update = 
# 117 "parser.mly"
        ( m )
# 575 "parser.ml"
                 in
                _menhir_goto_update _menhir_env _menhir_stack _v) : 'freshtv198)) : 'freshtv200)
            | MenhirState7 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv203 * _menhir_state * (
# 17 "parser.mly"
       (string)
# 583 "parser.ml"
                )) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_method_def) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv201 * _menhir_state * (
# 17 "parser.mly"
       (string)
# 591 "parser.ml"
                )) = Obj.magic _menhir_stack in
                let (_ : _menhir_state) = _menhir_s in
                let (m : 'tv_method_def) = _v in
                ((let (_menhir_stack, _menhir_s, label) = _menhir_stack in
                let _v : 'tv_method_decl = 
# 131 "parser.mly"
     ( label, m )
# 599 "parser.ml"
                 in
                _menhir_goto_method_decl _menhir_env _menhir_stack _menhir_s _v) : 'freshtv202)) : 'freshtv204)
            | _ ->
                _menhir_fail ()) : 'freshtv206)) : 'freshtv208)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv209 * _menhir_state) * (
# 17 "parser.mly"
       (string)
# 611 "parser.ml"
            )) * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv210)) : 'freshtv212)) : 'freshtv214)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv221 * _menhir_state * (
# 17 "parser.mly"
       (string)
# 620 "parser.ml"
        )) * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv219 * _menhir_state * (
# 17 "parser.mly"
       (string)
# 628 "parser.ml"
        )) * _menhir_state * 'tv_term) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DOT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | RBRACKET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv215 * _menhir_state * (
# 17 "parser.mly"
       (string)
# 643 "parser.ml"
            )) * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, label), _, m) = _menhir_stack in
            let _v : 'tv_method_decl = 
# 133 "parser.mly"
            ( label, (None, m))
# 649 "parser.ml"
             in
            _menhir_goto_method_decl _menhir_env _menhir_stack _menhir_s _v) : 'freshtv216)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv217 * _menhir_state * (
# 17 "parser.mly"
       (string)
# 659 "parser.ml"
            )) * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv218)) : 'freshtv220)) : 'freshtv222)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv231 * _menhir_state) * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv229 * _menhir_state) * _menhir_state * 'tv_term) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DOT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv225 * _menhir_state) * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv223 * _menhir_state) * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, t) = _menhir_stack in
            let _v : 'tv_term0 = 
# 77 "parser.mly"
 ( t )
# 686 "parser.ml"
             in
            _menhir_goto_term0 _menhir_env _menhir_stack _menhir_s _v) : 'freshtv224)) : 'freshtv226)
        | STAR ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv227 * _menhir_state) * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv228)) : 'freshtv230)) : 'freshtv232)
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv253 * _menhir_state * (
# 17 "parser.mly"
       (string)
# 703 "parser.ml"
        )) * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv251 * _menhir_state * (
# 17 "parser.mly"
       (string)
# 711 "parser.ml"
        )) * _menhir_state * 'tv_term) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DOT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | AND | IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv247 * _menhir_state * (
# 17 "parser.mly"
       (string)
# 726 "parser.ml"
            )) * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, label), _, t) = _menhir_stack in
            let _v : 'tv_def = 
# 66 "parser.mly"
                              (  label, t )
# 732 "parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv245) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_def) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv243 * _menhir_state * 'tv_def) = Obj.magic _menhir_stack in
            ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            let _tok = _menhir_env._menhir_token in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv241 * _menhir_state * 'tv_def) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | AND ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv235 * _menhir_state * 'tv_def) = Obj.magic _menhir_stack in
                ((let _tok = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv233 * _menhir_state * 'tv_def) = _menhir_stack in
                let (_tok : token) = _tok in
                ((match _tok with
                | IDENT _v ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52) : 'freshtv234)) : 'freshtv236)
            | IN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv237 * _menhir_state * 'tv_def) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, d) = _menhir_stack in
                let _v : 'tv_def_list = 
# 61 "parser.mly"
           ( [d] )
# 768 "parser.ml"
                 in
                _menhir_goto_def_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv238)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv239 * _menhir_state * 'tv_def) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv240)) : 'freshtv242)) : 'freshtv244)) : 'freshtv246)) : 'freshtv248)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv249 * _menhir_state * (
# 17 "parser.mly"
       (string)
# 785 "parser.ml"
            )) * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv250)) : 'freshtv252)) : 'freshtv254)
    | MenhirState57 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv261 * 'tv_defs) * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv259 * 'tv_defs) * _menhir_state * 'tv_term) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DOT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv255 * 'tv_defs) * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, d), _, t) = _menhir_stack in
            let _v : 'tv_program = 
# 52 "parser.mly"
                       ( (d, t) )
# 811 "parser.ml"
             in
            _menhir_goto_program _menhir_env _menhir_stack _v) : 'freshtv256)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv257 * 'tv_defs) * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv258)) : 'freshtv260)) : 'freshtv262)
    | _ ->
        _menhir_fail ()

and _menhir_goto_method_list : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_method_list -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv159 * _menhir_state) * _menhir_state * 'tv_method_list) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv157 * _menhir_state) * _menhir_state * 'tv_method_list) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | RBRACKET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv153 * _menhir_state) * _menhir_state * 'tv_method_list) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv151 * _menhir_state) * _menhir_state * 'tv_method_list) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, ms) = _menhir_stack in
            let _v : 'tv_term0 = 
# 83 "parser.mly"
 ( PObject ms )
# 847 "parser.ml"
             in
            _menhir_goto_term0 _menhir_env _menhir_stack _menhir_s _v) : 'freshtv152)) : 'freshtv154)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv155 * _menhir_state) * _menhir_state * 'tv_method_list) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv156)) : 'freshtv158)) : 'freshtv160)
    | MenhirState44 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv163 * _menhir_state * 'tv_method_decl) * _menhir_state * 'tv_method_list) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv161 * _menhir_state * 'tv_method_decl) * _menhir_state * 'tv_method_list) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, m), _, ms) = _menhir_stack in
        let _v : 'tv_method_list = 
# 126 "parser.mly"
                                         (  m :: ms )
# 866 "parser.ml"
         in
        _menhir_goto_method_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv162)) : 'freshtv164)
    | _ ->
        _menhir_fail ()

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv149 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv145 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv143 * _menhir_state) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | IDENT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv139 * _menhir_state) = Obj.magic _menhir_stack in
            let (_v : (
# 17 "parser.mly"
       (string)
# 894 "parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv137 * _menhir_state) * (
# 17 "parser.mly"
       (string)
# 902 "parser.ml"
            )) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | RPAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv133 * _menhir_state) * (
# 17 "parser.mly"
       (string)
# 911 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let _tok = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv131 * _menhir_state) * (
# 17 "parser.mly"
       (string)
# 918 "parser.ml"
                )) = _menhir_stack in
                let (_tok : token) = _tok in
                ((match _tok with
                | CONST_BOOL _v ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
                | CONST_INT _v ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
                | CONST_REAL _v ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
                | IDENT _v ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
                | LAMBDA ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState11
                | LBRACKET ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState11
                | LPAREN ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState11
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11) : 'freshtv132)) : 'freshtv134)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv135 * _menhir_state) * (
# 17 "parser.mly"
       (string)
# 947 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv136)) : 'freshtv138)) : 'freshtv140)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv141 * _menhir_state) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv142)) : 'freshtv144)) : 'freshtv146)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv147 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv148)) : 'freshtv150)

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_term1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_term1 -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv129 * _menhir_state * 'tv_term1) = Obj.magic _menhir_stack in
    ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv127 * _menhir_state * 'tv_term1) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | CONST_BOOL _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | CONST_INT _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | CONST_REAL _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | IDENT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | LBRACKET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | LPAREN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | AND | COMMA | DOT | EOF | IN | PLUS | RBRACKET | RPAREN | STAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv125 * _menhir_state * 'tv_term1) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, t) = _menhir_stack in
        let _v : 'tv_term = 
# 99 "parser.mly"
 ( t )
# 1001 "parser.ml"
         in
        _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v) : 'freshtv126)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20) : 'freshtv128)) : 'freshtv130)

and _menhir_reduce15 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_method_list = 
# 123 "parser.mly"
              ( [] )
# 1014 "parser.ml"
     in
    _menhir_goto_method_list _menhir_env _menhir_stack _menhir_s _v

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 17 "parser.mly"
       (string)
# 1021 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv123 * _menhir_state * (
# 17 "parser.mly"
       (string)
# 1030 "parser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | EQUAL ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv119 * _menhir_state * (
# 17 "parser.mly"
       (string)
# 1039 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv117 * _menhir_state * (
# 17 "parser.mly"
       (string)
# 1046 "parser.ml"
        )) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | CONST_BOOL _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
        | CONST_INT _v ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
        | CONST_REAL _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
        | IDENT _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
        | LAMBDA ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState7
        | LBRACKET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState7
        | LPAREN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState7
        | SIGMA ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState7
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7) : 'freshtv118)) : 'freshtv120)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv121 * _menhir_state * (
# 17 "parser.mly"
       (string)
# 1077 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv122)) : 'freshtv124)

and _menhir_goto_term0 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_term0 -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv111 * _menhir_state * 'tv_term1) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_term0) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv109 * _menhir_state * 'tv_term1) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (t2 : 'tv_term0) = _v in
        ((let (_menhir_stack, _menhir_s, t1) = _menhir_stack in
        let _v : 'tv_term1 = 
# 94 "parser.mly"
 ( PApply (t1, t2) )
# 1098 "parser.ml"
         in
        _menhir_goto_term1 _menhir_env _menhir_stack _menhir_s _v) : 'freshtv110)) : 'freshtv112)
    | MenhirState57 | MenhirState3 | MenhirState4 | MenhirState7 | MenhirState11 | MenhirState34 | MenhirState32 | MenhirState25 | MenhirState15 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv115) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_term0) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv113) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (t : 'tv_term0) = _v in
        ((let _v : 'tv_term1 = 
# 88 "parser.mly"
    ( t )
# 1113 "parser.ml"
         in
        _menhir_goto_term1 _menhir_env _menhir_stack _menhir_s _v) : 'freshtv114)) : 'freshtv116)
    | _ ->
        _menhir_fail ()

and _menhir_goto_const : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_const -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv107) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_const) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv105) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (c : 'tv_const) = _v in
    ((let _v : 'tv_term0 = 
# 81 "parser.mly"
 ( PConst c )
# 1132 "parser.ml"
     in
    _menhir_goto_term0 _menhir_env _menhir_stack _menhir_s _v) : 'freshtv106)) : 'freshtv108)

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv103 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | CONST_BOOL _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | CONST_INT _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | CONST_REAL _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | IDENT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | LAMBDA ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | LBRACKET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | LPAREN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4) : 'freshtv104)

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv101 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | IDENT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | RBRACKET ->
        _menhir_reduce15 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5) : 'freshtv102)

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv99 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv95 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv93 * _menhir_state) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | IDENT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv89 * _menhir_state) = Obj.magic _menhir_stack in
            let (_v : (
# 17 "parser.mly"
       (string)
# 1202 "parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv87 * _menhir_state) * (
# 17 "parser.mly"
       (string)
# 1210 "parser.ml"
            )) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | RPAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv83 * _menhir_state) * (
# 17 "parser.mly"
       (string)
# 1219 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let _tok = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv81 * _menhir_state) * (
# 17 "parser.mly"
       (string)
# 1226 "parser.ml"
                )) = _menhir_stack in
                let (_tok : token) = _tok in
                ((match _tok with
                | CONST_BOOL _v ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
                | CONST_INT _v ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
                | CONST_REAL _v ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
                | IDENT _v ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
                | LAMBDA ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState15
                | LBRACKET ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState15
                | LPAREN ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState15
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15) : 'freshtv82)) : 'freshtv84)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv85 * _menhir_state) * (
# 17 "parser.mly"
       (string)
# 1255 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)) : 'freshtv88)) : 'freshtv90)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv91 * _menhir_state) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv92)) : 'freshtv94)) : 'freshtv96)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv97 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)) : 'freshtv100)

and _menhir_run16 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 17 "parser.mly"
       (string)
# 1277 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv79) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (i : (
# 17 "parser.mly"
       (string)
# 1287 "parser.ml"
    )) = _v in
    ((let _v : 'tv_term0 = 
# 79 "parser.mly"
 ( PVariable i )
# 1292 "parser.ml"
     in
    _menhir_goto_term0 _menhir_env _menhir_stack _menhir_s _v) : 'freshtv80)

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 13 "parser.mly"
       (float)
# 1299 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv77) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (c : (
# 13 "parser.mly"
       (float)
# 1309 "parser.ml"
    )) = _v in
    ((let _v : 'tv_const = 
# 72 "parser.mly"
                 ( Preal c )
# 1314 "parser.ml"
     in
    _menhir_goto_const _menhir_env _menhir_stack _menhir_s _v) : 'freshtv78)

and _menhir_run18 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 12 "parser.mly"
       (int)
# 1321 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv75) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (c : (
# 12 "parser.mly"
       (int)
# 1331 "parser.ml"
    )) = _v in
    ((let _v : 'tv_const = 
# 71 "parser.mly"
                (  Pint c )
# 1336 "parser.ml"
     in
    _menhir_goto_const _menhir_env _menhir_stack _menhir_s _v) : 'freshtv76)

and _menhir_run19 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 11 "parser.mly"
       (bool)
# 1343 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv73) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (c : (
# 11 "parser.mly"
       (bool)
# 1353 "parser.ml"
    )) = _v in
    ((let _v : 'tv_const = 
# 70 "parser.mly"
                 ( Pbool c )
# 1358 "parser.ml"
     in
    _menhir_goto_const _menhir_env _menhir_stack _menhir_s _v) : 'freshtv74)

and _menhir_goto_defs : _menhir_env -> 'ttv_tail -> 'tv_defs -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv71 * 'tv_defs) = Obj.magic _menhir_stack in
    ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv69 * 'tv_defs) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | CONST_BOOL _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | CONST_INT _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | CONST_REAL _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | IDENT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | LAMBDA ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | LBRACKET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | LPAREN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57) : 'freshtv70)) : 'freshtv72)

and _menhir_goto_program : _menhir_env -> 'ttv_tail -> 'tv_program -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv67 * 'tv_program) = Obj.magic _menhir_stack in
    ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv65 * 'tv_program) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | EOF ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv61 * 'tv_program) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv59 * 'tv_program) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, p) = _menhir_stack in
        let _v : (
# 43 "parser.mly"
      ((string * Parsed_terms.p_term_t) list * Parsed_terms.p_term_t)
# 1412 "parser.ml"
        ) = 
# 47 "parser.mly"
                      ( p )
# 1416 "parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv57) = _menhir_stack in
        let (_v : (
# 43 "parser.mly"
      ((string * Parsed_terms.p_term_t) list * Parsed_terms.p_term_t)
# 1423 "parser.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv55) = Obj.magic _menhir_stack in
        let (_v : (
# 43 "parser.mly"
      ((string * Parsed_terms.p_term_t) list * Parsed_terms.p_term_t)
# 1430 "parser.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv53) = Obj.magic _menhir_stack in
        let (_1 : (
# 43 "parser.mly"
      ((string * Parsed_terms.p_term_t) list * Parsed_terms.p_term_t)
# 1437 "parser.ml"
        )) = _v in
        (Obj.magic _1 : 'freshtv54)) : 'freshtv56)) : 'freshtv58)) : 'freshtv60)) : 'freshtv62)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv63 * 'tv_program) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv64)) : 'freshtv66)) : 'freshtv68)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState57 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv23 * 'tv_defs) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv24)
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv25 * _menhir_state * 'tv_def) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)
    | MenhirState44 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv27 * _menhir_state * 'tv_method_decl) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv28)
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv29 * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv30)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv31) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv32)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv33) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv34)
    | MenhirState25 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv35 * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv36)
    | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv37 * _menhir_state * 'tv_term1) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)
    | MenhirState15 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv39 * _menhir_state) * (
# 17 "parser.mly"
       (string)
# 1492 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)
    | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv41 * _menhir_state) * (
# 17 "parser.mly"
       (string)
# 1501 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv43 * _menhir_state * (
# 17 "parser.mly"
       (string)
# 1510 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)
    | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv45 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv47 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv48)
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv49 * _menhir_state * (
# 17 "parser.mly"
       (string)
# 1529 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv51) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv52)

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 17 "parser.mly"
       (string)
# 1541 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv21 * _menhir_state * (
# 17 "parser.mly"
       (string)
# 1550 "parser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | EQUAL ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv17 * _menhir_state * (
# 17 "parser.mly"
       (string)
# 1559 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv15 * _menhir_state * (
# 17 "parser.mly"
       (string)
# 1566 "parser.ml"
        )) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | CONST_BOOL _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
        | CONST_INT _v ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
        | CONST_REAL _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
        | IDENT _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
        | LAMBDA ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | LBRACKET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | LPAREN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3) : 'freshtv16)) : 'freshtv18)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv19 * _menhir_state * (
# 17 "parser.mly"
       (string)
# 1595 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv20)) : 'freshtv22)

and _menhir_discard : _menhir_env -> token =
  fun _menhir_env ->
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = _menhir_env._menhir_lexer lexbuf in
    _menhir_env._menhir_token <- _tok;
    _menhir_env._menhir_startp <- lexbuf.Lexing.lex_start_p;
    _menhir_env._menhir_endp <- lexbuf.Lexing.lex_curr_p;
    let shifted = Pervasives.(+) _menhir_env._menhir_shifted 1 in
    if Pervasives.(>=) shifted 0 then
      _menhir_env._menhir_shifted <- shifted;
    _tok

and file : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 43 "parser.mly"
      ((string * Parsed_terms.p_term_t) list * Parsed_terms.p_term_t)
# 1615 "parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env =
      let (lexer : Lexing.lexbuf -> token) = lexer in
      let (lexbuf : Lexing.lexbuf) = lexbuf in
      ((let _tok = lexer lexbuf in
      {
        _menhir_lexer = lexer;
        _menhir_lexbuf = lexbuf;
        _menhir_token = _tok;
        _menhir_startp = lexbuf.Lexing.lex_start_p;
        _menhir_endp = lexbuf.Lexing.lex_curr_p;
        _menhir_shifted = max_int;
        }) : _menhir_env)
    in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv13) = () in
    ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv11) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | LET ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv3) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | IDENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState1) : 'freshtv2)) : 'freshtv4)
    | EOF ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv5) = Obj.magic _menhir_stack in
        ((let _v : 'tv_program = 
# 51 "parser.mly"
               ( ([], PObject []) )
# 1659 "parser.ml"
         in
        _menhir_goto_program _menhir_env _menhir_stack _v) : 'freshtv6)
    | CONST_BOOL _ | CONST_INT _ | CONST_REAL _ | IDENT _ | LAMBDA | LBRACKET | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv7) = Obj.magic _menhir_stack in
        ((let _v : 'tv_defs = 
# 56 "parser.mly"
              ( [] )
# 1668 "parser.ml"
         in
        _menhir_goto_defs _menhir_env _menhir_stack _v) : 'freshtv8)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv9) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv10)) : 'freshtv12)) : 'freshtv14))



