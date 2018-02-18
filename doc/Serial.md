
## Serialized Format

File    ::= '53' '4d' '52' '31' Seq[Decl]       (Shimmer File: "SMR1" in ASCII, then Decls)

Decl    ::= (dmac)    'd0' Name Exp             (Macro declaration)
         |  (dset)    'd1' Name Exp             (Set declaration)

Var     ::= (var)     '8N' Word8^N              (Short Varible,       N <= 15)

Abs     ::= (abs)     '9N' Exp^N                (Short Abstraction,   N <= 15)

App     ::= (app)     'aN' Exp^N                (Packed Application,  N <= 15)

Exp     ::= (ref)     'b0' Ref                  (External reference)
         |  (key)     'b1' Key Exp              (Keyword  application)
         |  (app)     'b2' Exp Seq[Exp]         (Function application)
         |  (var)     'b3' Name Bump            (Variable with bump counter)
         |  (abs)     'b4' Seq[Param] Exp       (Function abstraction)
         |  (sub)     'b5' Seq[Car] Exp         (Substitution train)
         |  Ref                                 (Short circuit to Ref)
         |  App                                 (Short circuit to App)
         |  Var                                 (Short circuit to Var)
         |  Abs                                 (Short circuit to Abs)

Key     ::= (box)     'b6'                      (Box keyword)
         |  (run)     'b7'                      (Run keyword)

Param   ::= (pvl)     'b8' Name                 (call-by-value parameter)
         |  (pna)     'b9' Name                 (call-by-name  parameter)

Car     ::= (csim)    'ba' Seq[SnvBind]         (Simultaneous substitution)
         |  (crec)    'bb' Seq[SnvBind]         (Recursive substitution)
         |  (cups)    'bc' Seq[UpsBump]         (Lifting specifiers)

SnvBind ::= (svar)    'bd' Name Bump Exp        (Substitute for variable)
         |  (snom)    'be' Nom Exp              (Substitute for nominal reference)

UpsBump ::= (up)      'bf' Name Bump Bump       (Lifting specifier)

Ref     ::= (sym)     'c0' Seq[Word8]           (Symbol reference)
         |  (prm)     'c1' Seq[Word8]           (Primitive reference)
         |  (txt)     'c2' Seq[Word8]           (Text reference)
         |  (mac)     'c3' Seq[Word8]           (Macro reference)
         |  (set)     'c4' Seq[Word8]           (Set reference)
         |  (nom)     'c5' Nom                  (Nominal reference)
         |  Name                                (Short circuit to Sym Name)

Prim    ::= (unit)    'e0'                      (Unit value)
         |  (list)    'e1'                      (List constructor tag)
         |  (true)    'e2'                      (True value)
         |  (false)   'e3'                      (False value)

         |  (word8)   'e4' Word8                ( 8-bit word value)
         |  (word16)  'e5' Word16               (16-bit word value)
         |  (word32)  'e6' Word32               (32-bit word value)
         |  (word64)  'e7' Word64               (64-bit word value)

         |  (int8)    'e8' Int8                 ( 8-bit  int value)
         |  (int16)   'e9' Int16                (16-bit  int value)
         |  (int32)   'ea' Int32                (32-bit  int value)
         |  (int64)   'eb' Int64                (64-bit  int value)

         |  (float32) 'ec' Float32              (32-bit float value)
         |  (float64) 'ed' Float64              (64-bit float value)

         |  (named)   'ee' Name                 (Named primitive)
         |  (words)   'ef' Name Seq[Word8]      (Packed raw words with type name)

Seq[A]  ::= (seqN)    'fN' A*                   (N-count then sequence of A things, N <= 13)
         |  (seq8)    'fd' Word8  A*            ( 8-bit count then sequence of A things)
         |  (seq16)   'fe' Word16 A*            (16-bit count then sequence of A things)
         |  (seq32)   'ff' Word32 A*            (32-bit count then sequence of A things)

Name    ::= Seq[Word8]                          (Name)

Bump    ::= Word16                              (Bump counter)

Nom     ::= Word32                              (Nominal constant)
