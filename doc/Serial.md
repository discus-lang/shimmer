
## Serialized Format

File    ::= '53' '4d' '52' '31' Seq[Decl]       (Shimmer File: "SMR1" in ASCII, then decls)

Decl    ::= (dmac)  '01' Name Exp               (Macro declaration)
         |  (dset)  '02' Name Exp               (Set declaration)

Seq[A]  ::= (seq8)  'f1' Word8  A*              ( 8-bit count then sequence of A things)
         |  (seq16) 'f2' Word16 A*              (16-bit count then sequence of A things)
         |  (seq32) 'f3' Word32 A*              (32-bit count then sequence of A things)

Name    ::= Seq[Word8]                          (Name)

Bump    ::= Nat16                               (Bump counter)

Nom     ::= Nat32                               (Nominal atom)

Ref     ::= (sym)   '11' Seq[Word8]             (Symbol reference)
         |  (prm)   '12' Seq[Word8]             (Primitive reference)
         |  (mac)   '13' Seq[Word8]             (Macro reference)
         |  (set)   '14' Seq[Word8]             (Set reference)
         |  (nom)   '15' Nom                    (Nominal reference)

Exp     ::= (ref)   '21' Ref                    (External reference)
         |  (key)   '22' Key Exp                (Keyword  application)
         |  (app)   '23' Exp Seq[Exp]           (Function application)
         |  (var)   '24' Name Bump              (Variable with lifting specifier)
         |  (abs)   '25' Seq[Param] Exp         (Function abstraction)
         |  (sub)   '26' Seq[Car] Exp           (Substitution train)

Key     ::= (box)   '2a'                        (Box keyword)
         |  (run)   '2b'                        (Run keyword)

Param   ::= (pvl)   '2c' Name                   (call-by-value parameter)
         |  (pna)   '2d' Name                   (call-by-name  parameter)

Car     ::= (csim)  '31' Seq[SnvBind]           (Simultaneous substitution)
         |  (crec)  '32' Seq[SnvBind]           (Recursive substitution)
         |  (cups)  '33' Seq[UpsBump]           (Lifting specifiers)

SnvBind ::= (svar)  '3a' Name Bump Exp          (Substitute for variable)
         |  (snom)  '3b' Nom Exp                (Substitute for nominal reference)

UpsBump ::= (up)    '3c' Name Bump Bump         (Lifting specifier)

