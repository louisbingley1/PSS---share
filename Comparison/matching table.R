# S(Z): Indicator of ICE a subject is assigned to treatment Z
# D(Z): The actual treatment taken when a subject is assigned to treatment Z
#
#       [U]              |   Stratum 1       Stratum 2      Stratum 3      Stratum 4       
#                        |    [01]             [11]          [00]            [10]
#------------------------|-------------------------------------------------------------------
#   AdACE PS             |  Control-only      Never-         Always-      Experimental-only
#                        |  compliers       compliers       compliers       compliers
#                        |   [adhpbo]         [adhnei]      [adhboth]       [adhact]       
#   [S(0),S(1)]          |    [01]             [11]          [00]            [10]
#------------------------|-------------------------------------------------------------------
#   Magnusson PS         |  Harmed            Doomed        Immune          Benefiters 
#                        |    [H]              [D]           [I]             [B]
#   [S(0),S(1)]          |    [01]             [11]          [00]            [10]
#------------------------|-------------------------------------------------------------------
#   Principal Score PS   |  Never-taker       Defiers        Compliers     Always-taker
#                        |    [N]              [D]            [c]             [A]
#   [D(0),D(1)]          |    [00]             [10]           [01]            [11]      
