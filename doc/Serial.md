
## Serialized Format

File    ::= '53' '4d' '52' '31' Seq[Decl]       (Shimmer File: "SMR1" in ASCII, then decls)

Decl    ::= (dmac)    'a1' Name Exp             (Macro declaration)
         |  (dset)    'a2' Name Exp             (Set declaration)

Exp     ::= (ref)     'b1' Ref                  (External reference)
         |  (key)     'b2' Key Exp              (Keyword  application)
         |  (app)     'b3' Exp Seq[Exp]         (Function application)
         |  (var)     'b4' Name Bump            (Variable with lifting specifier)
         |  (abs)     'b5' Seq[Param] Exp       (Function abstraction)
         |  (sub)     'b6' Seq[Car] Exp         (Substitution train)

Key     ::= (box)     'ba'                      (Box keyword)
         |  (run)     'bb'                      (Run keyword)

Param   ::= (pvl)     'bc' Name                 (call-by-value parameter)
         |  (pna)     'bd' Name                 (call-by-name  parameter)

Car     ::= (csim)    'c1' Seq[SnvBind]         (Simultaneous substitution)
         |  (crec)    'c2' Seq[SnvBind]         (Recursive substitution)
         |  (cups)    'c3' Seq[UpsBump]         (Lifting specifiers)

SnvBind ::= (svar)    'ca' Name Bump Exp        (Substitute for variable)
         |  (snom)    'cb' Nom Exp              (Substitute for nominal reference)

UpsBump ::= (up)      'cc' Name Bump Bump       (Lifting specifier)

Ref     ::= (sym)     'd1' Seq[Word8]           (Symbol reference)
         |  (prm)     'd2' Seq[Word8]           (Primitive reference)
         |  (mac)     'd3' Seq[Word8]           (Macro reference)
         |  (set)     'd4' Seq[Word8]           (Set reference)
         |  (nom)     'd5' Nom                  (Nominal reference)

Prim    ::= (unit)    'da'
         |  (true)    'db'
         |  (false)   'dc'
         |  (text)    'df' Name

         |  (word8)   'e1' Word8
         |  (word16)  'e2' Word16
         |  (word32)  'e3' Word32
         |  (word64)  'e4' Word64

         |  (int8)    'e5' Int8
         |  (int16)   'e6' Int16
         |  (int32)   'e7' Int32
         |  (int64)   'e8' Int64

         |  (float32) 'ea' Float32
         |  (float64) 'eb' Float64

         |  (words)   'ef' Name Seq[Word8]


Seq[A]  ::= (seq8)    'f1' Word8  A*            ( 8-bit count then sequence of A things)
         |  (seq16)   'f2' Word16 A*            (16-bit count then sequence of A things)
         |  (seq32)   'f3' Word32 A*            (32-bit count then sequence of A things)

Name    ::= Seq[Word8]                          (Name)

Bump    ::= Word16                              (Bump counter)

Nom     ::= Word32                              (Nominal atom)
