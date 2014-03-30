module Builtin where
import Data.List (find)
import Data.Char (ord)
import Syntax

churchnums = [
 SLit $ LitStr "`ki", -- 0
 SLit $ LitStr "i", -- 1
 SLit $ LitStr "``s``s`kski", -- 2
 SLit $ LitStr "``s``s`ksk``s``s`kski", -- 3
 SLit $ LitStr "```sii``s``s`kski", -- 4
 SLit $ LitStr "``s``s`ksk```sii``s``s`kski", -- 5
 SLit $ LitStr "``s``s`ksk``s``s`ksk```sii``s``s`kski", -- 6
 SLit $ LitStr "``s``s`ksk``s``s`ksk``s``s`ksk```sii``s``s`kski", -- 7
 SLit $ LitStr "``s`k``s``s`kski```sii``s``s`kski", -- 8
 SLit $ LitStr "```s``s`kski``s``s`ksk``s``s`kski", -- 9
 SLit $ LitStr "``s`k``s``s`kski``s``s`ksk```sii``s``s`kski", -- 10
 SLit $ LitStr "````s``s`kski```s``s`ksk```sii``s``s`kski`s``s`kski", -- 11
 SLit $ LitStr "``s`k``s``s`ksk``s``s`kski```sii``s``s`kski", -- 12
 SLit $ LitStr "````s``s`ksk``s``s`kski````sii``s``s`kski`s``s`kski", -- 13
 SLit $ LitStr "``s``s`ksk````s``s`ksk``s``s`kski````sii``s``s`kski`s``s`kski", -- 14
 SLit $ LitStr "``s`k``s``s`ksk``s``s`kski``s``s`ksk```sii``s``s`kski", -- 15
 SLit $ LitStr "```s``siii``s``s`kski", -- 16
 SLit $ LitStr "``s``s`ksk```s``siii``s``s`kski", -- 17
 SLit $ LitStr "``s``s`ksk``s``s`ksk```s``siii``s``s`kski", -- 18
 SLit $ LitStr "``s``s`ksk``s``s`ksk``s``s`ksk```s``siii``s``s`kski", -- 19
 SLit $ LitStr "`````sii``s``s`kski`s``s`ksk```s``siii``s``s`kski", -- 20
 SLit $ LitStr "``s``s`ksk`````sii``s``s`kski`s``s`ksk```s``siii``s``s`kski", -- 21
 SLit $ LitStr "``s`k``s``s`kski````s``s`kski```s``s`ksk```sii``s``s`kski`s``s`kski", -- 22
 SLit $ LitStr "````s``s`kski`````s``s`kski```s``s`ksk```sii``s``s`kski`s``s`kski`s``s`kski", -- 23
 SLit $ LitStr "````s``s`kski````sii``s``s`kski`s``s`ksk```s``siii``s``s`kski", -- 24
 SLit $ LitStr "```s``s`kski``s``s`ksk```sii``s``s`kski", -- 25
 SLit $ LitStr "``s``s`ksk```s``s`kski``s``s`ksk```sii``s``s`kski", -- 26
 SLit $ LitStr "```sii``s``s`ksk``s``s`kski", -- 27
 SLit $ LitStr "``s``s`ksk```sii``s``s`ksk``s``s`kski", -- 28
 SLit $ LitStr "``s``s`ksk``s``s`ksk```sii``s``s`ksk``s``s`kski", -- 29
 SLit $ LitStr "``s``s`ksk``s``s`ksk``s``s`ksk```sii``s``s`ksk``s``s`kski", -- 30
 SLit $ LitStr "`````sii``s``s`kski`s``s`ksk```sii``s``s`ksk``s``s`kski", -- 31
 SLit $ LitStr "``s`k``s``s`kski```s``siii``s``s`kski", -- 32
 SLit $ LitStr "````s``s`kski````s``siii``s``s`kski`s``s`kski", -- 33
 SLit $ LitStr "``s`k``s``s`kski``s``s`ksk```s``siii``s``s`kski", -- 34
 SLit $ LitStr "````s``s`kski```s``s`ksk```s``siii``s``s`kski`s``s`kski", -- 35
 SLit $ LitStr "```s``s`kski``s``s`ksk``s``s`ksk```sii``s``s`kski", -- 36
 SLit $ LitStr "``s``s`ksk```s``s`kski``s``s`ksk``s``s`ksk```sii``s``s`kski", -- 37
 SLit $ LitStr "``s`k``s``s`kski``s``s`ksk``s``s`ksk``s``s`ksk```s``siii``s``s`kski", -- 38
 SLit $ LitStr "````s``s`kski```s``s`ksk``s``s`ksk``s``s`ksk```s``siii``s``s`kski`s``s`kski", -- 39
 SLit $ LitStr "``s`k``s``s`kski`````sii``s``s`kski`s``s`ksk```s``siii``s``s`kski", -- 40
 SLit $ LitStr "`````s``siii``s``s`kski`s``s`ksk```s``s`kski``s``s`ksk```sii``s``s`kski", -- 41
 SLit $ LitStr "``s`k``s``s`kski``s``s`ksk`````sii``s``s`kski`s``s`ksk```s``siii``s``s`kski", -- 42
 SLit $ LitStr "`````s``siii``s``s`kski`s``s`ksk```sii``s``s`ksk``s``s`kski", -- 43
 SLit $ LitStr "``s``s`ksk`````s``siii``s``s`kski`s``s`ksk```sii``s``s`ksk``s``s`kski", -- 44
 SLit $ LitStr "``s`k``s``s`ksk```sii``s``s`kski```s``s`kski``s``s`ksk``s``s`kski", -- 45
 SLit $ LitStr "````s``s`ksk```sii``s``s`kski````s``s`kski``s``s`ksk``s``s`kski`s``s`kski", -- 46
 SLit $ LitStr "``s``s`ksk````s``s`ksk```sii``s``s`kski````s``s`kski``s``s`ksk``s``s`kski`s``s`kski", -- 47
 SLit $ LitStr "``s`k``s``s`ksk``s``s`kski```s``siii``s``s`kski", -- 48
 SLit $ LitStr "````s``s`ksk``s``s`kski````s``siii``s``s`kski`s``s`kski", -- 49
 SLit $ LitStr "``s`k``s``s`kski```s``s`kski``s``s`ksk```sii``s``s`kski", -- 50
 SLit $ LitStr "``s`k``s``s`ksk``s``s`kski``s``s`ksk```s``siii``s``s`kski", -- 51
 SLit $ LitStr "````s``s`ksk``s``s`kski```s``s`ksk```s``siii``s``s`kski`s``s`kski", -- 52
 SLit $ LitStr "``s``s`ksk````s``s`ksk``s``s`kski```s``s`ksk```s``siii``s``s`kski`s``s`kski", -- 53
 SLit $ LitStr "``s`k``s``s`kski```sii``s``s`ksk``s``s`kski", -- 54
 SLit $ LitStr "````s``s`kski````sii``s``s`ksk``s``s`kski`s``s`kski", -- 55
 SLit $ LitStr "``s`k``s``s`kski``s``s`ksk```sii``s``s`ksk``s``s`kski", -- 56
 SLit $ LitStr "````s``s`kski```s``s`ksk```sii``s``s`ksk``s``s`kski`s``s`kski", -- 57
 SLit $ LitStr "``s`k``s``s`kski``s``s`ksk``s``s`ksk```sii``s``s`ksk``s``s`kski", -- 58
 SLit $ LitStr "````s``s`kski````s``siii``s``s`kski`s``s`ksk```sii``s``s`ksk``s``s`kski", -- 59
 SLit $ LitStr "``s`k``s``s`ksk``s``s`kski`````sii``s``s`kski`s``s`ksk```s``siii``s``s`kski", -- 60
 SLit $ LitStr "````s``s`kski```s``s`ksk```s``siii``s``s`kski`s``s`ksk```sii``s``s`ksk``s``s`kski", -- 61
 SLit $ LitStr "``s`k``s``s`kski`````sii``s``s`kski`s``s`ksk```sii``s``s`ksk``s``s`kski", -- 62
 SLit $ LitStr "````s``s`kski``````sii``s``s`kski`s``s`ksk```sii``s``s`ksk``s``s`kski`s``s`kski", -- 63
 SLit $ LitStr "```s`s``s`ksk``sii``s``s`kski", -- 64
 SLit $ LitStr "``s``s`ksk```s`s``s`ksk``sii``s``s`kski", -- 65
 SLit $ LitStr "``s``s`ksk``s``s`ksk```s`s``s`ksk``sii``s``s`kski", -- 66
 SLit $ LitStr "``s``s`ksk``s``s`ksk``s``s`ksk```s`s``s`ksk``sii``s``s`kski", -- 67
 SLit $ LitStr "``s`k```sii``s``s`kski``s``s`ksk```s``siii``s``s`kski", -- 68
 SLit $ LitStr "`````sii``s``s`kski```s``s`ksk```s``siii``s``s`kski`s``s`kski", -- 69
 SLit $ LitStr "``s``s`ksk`````sii``s``s`kski```s``s`ksk```s``siii``s``s`kski`s``s`kski", -- 70
 SLit $ LitStr "``s``s`ksk``s``s`ksk`````sii``s``s`kski```s``s`ksk```s``siii``s``s`kski`s``s`kski", -- 71
 SLit $ LitStr "``s`k```sii``s``s`kski``s``s`ksk``s``s`ksk```s``siii``s``s`kski", -- 72
 SLit $ LitStr "`````sii``s``s`kski```s``s`ksk``s``s`ksk```s``siii``s``s`kski`s``s`kski", -- 73
 SLit $ LitStr "``s`k``s``s`kski``s``s`ksk```s``s`kski``s``s`ksk``s``s`ksk```sii``s``s`kski", -- 74
 SLit $ LitStr "``s`k``s``s`ksk``s``s`kski```s``s`kski``s``s`ksk```sii``s``s`kski", -- 75
 SLit $ LitStr "````s``s`ksk``s``s`kski````s``s`kski``s``s`ksk```sii``s``s`kski`s``s`kski", -- 76
 SLit $ LitStr "``s``s`ksk````s``s`ksk``s``s`kski````s``s`kski``s``s`ksk```sii``s``s`kski`s``s`kski", -- 77
 SLit $ LitStr "``s`k``s``s`ksk``s``s`kski``s``s`ksk```s``s`kski``s``s`ksk```sii``s``s`kski", -- 78
 SLit $ LitStr "````s``s`ksk``s``s`kski```s``s`ksk```s``s`kski``s``s`ksk```sii``s``s`kski`s``s`kski", -- 79
 SLit $ LitStr "``s`k``s``s`ksk```sii``s``s`kski```s``siii``s``s`kski", -- 80
 SLit $ LitStr "```s``sii`s``s`ksk``s``s`kski", -- 81
 SLit $ LitStr "``s``s`ksk```s``sii`s``s`ksk``s``s`kski", -- 82
 SLit $ LitStr "``s``s`ksk``s``s`ksk```s``sii`s``s`ksk``s``s`kski", -- 83
 SLit $ LitStr "``s``s`ksk``s``s`ksk``s``s`ksk```s``sii`s``s`ksk``s``s`kski", -- 84
 SLit $ LitStr "`````sii``s``s`kski`s``s`ksk```s``sii`s``s`ksk``s``s`kski", -- 85
 SLit $ LitStr "``s``s`ksk`````sii``s``s`kski`s``s`ksk```s``sii`s``s`ksk``s``s`kski", -- 86
 SLit $ LitStr "``s`k``s``s`ksk``s``s`kski``s``s`ksk``s``s`ksk```sii``s``s`ksk``s``s`kski", -- 87
 SLit $ LitStr "````s``s`ksk``s``s`kski```s``s`ksk``s``s`ksk```sii``s``s`ksk``s``s`kski`s``s`kski", -- 88
 SLit $ LitStr "````s``s`kski````sii``s``s`kski`s``s`ksk```s``sii`s``s`ksk``s``s`kski", -- 89
 SLit $ LitStr "`````s``s`kski``s``s`ksk``s``s`kski`s``s`ksk```s``sii`s``s`ksk``s``s`kski", -- 90
 SLit $ LitStr "`````sii``s``s`ksk``s``s`kski`s``s`ksk```s`s``s`ksk``sii``s``s`kski", -- 91
 SLit $ LitStr "``s``s`ksk`````sii``s``s`ksk``s``s`kski`s``s`ksk```s`s``s`ksk``sii``s``s`kski", -- 92
 SLit $ LitStr "````s``s`ksk``s``s`kski````sii``s``s`kski`s``s`ksk```s``sii`s``s`ksk``s``s`kski", -- 93
 SLit $ LitStr "``s``s`ksk````s``s`ksk``s``s`kski````sii``s``s`kski`s``s`ksk```s``sii`s``s`ksk``s``s`kski", -- 94
 SLit $ LitStr "``s`k``s``s`ksk```sii``s``s`kski``s``s`ksk``s``s`ksk``s``s`ksk```s``siii``s``s`kski", -- 95
 SLit $ LitStr "``s`k``s``s`ksk``s``s`ksk```sii``s``s`kski```s``siii``s``s`kski", -- 96
 SLit $ LitStr "`````s``siii``s``s`kski`s``s`ksk```s``sii`s``s`ksk``s``s`kski", -- 97
 SLit $ LitStr "``s`k``s``s`kski````s``s`ksk``s``s`kski````s``siii``s``s`kski`s``s`kski", -- 98
 SLit $ LitStr "``s`k``s``s`ksk``s``s`kski````s``s`kski````s``siii``s``s`kski`s``s`kski", -- 99
 SLit $ LitStr "```s``s`kski``s`k``s``s`kski``s``s`ksk```sii``s``s`kski", -- 100
 SLit $ LitStr "``s``s`ksk```s``s`kski``s`k``s``s`kski``s``s`ksk```sii``s``s`kski", -- 101
 SLit $ LitStr "``s`k``s``s`ksk``s``s`ksk```sii``s``s`kski``s``s`ksk```s``siii``s``s`kski", -- 102
 SLit $ LitStr "````s``s`ksk``s``s`ksk```sii``s``s`kski```s``s`ksk```s``siii``s``s`kski`s``s`kski", -- 103
 SLit $ LitStr "``s`k```sii``s``s`kski``s``s`ksk```s``s`kski``s``s`ksk```sii``s``s`kski", -- 104
 SLit $ LitStr "`````sii``s``s`kski```s``s`ksk```s``s`kski``s``s`ksk```sii``s``s`kski`s``s`kski", -- 105
 SLit $ LitStr "`````s``s`kski``s``s`ksk```sii``s``s`kski`s``s`ksk```s``sii`s``s`ksk``s``s`kski", -- 106
 SLit $ LitStr "````s``s`ksk```sii``s``s`kski````s``siii``s``s`kski`s``s`ksk```sii``s``s`ksk``s``s`kski", -- 107
 SLit $ LitStr "``s`k```sii``s``s`kski```sii``s``s`ksk``s``s`kski", -- 108
 SLit $ LitStr "`````sii``s``s`kski````sii``s``s`ksk``s``s`kski`s``s`kski", -- 109
 SLit $ LitStr "``s``s`ksk`````sii``s``s`kski````sii``s``s`ksk``s``s`kski`s``s`kski", -- 110
 SLit $ LitStr "``s``s`ksk``s``s`ksk`````sii``s``s`kski````sii``s``s`ksk``s``s`kski`s``s`kski", -- 111
 SLit $ LitStr "``s`k```sii``s``s`kski``s``s`ksk```sii``s``s`ksk``s``s`kski", -- 112
 SLit $ LitStr "`````sii``s``s`kski```s``s`ksk```sii``s``s`ksk``s``s`kski`s``s`kski", -- 113
 SLit $ LitStr "``s``s`ksk`````sii``s``s`kski```s``s`ksk```sii``s``s`ksk``s``s`kski`s``s`kski", -- 114
 SLit $ LitStr "````s``s`kski```s``s`ksk```s``siii``s``s`kski`s``s`ksk```s``sii`s``s`ksk``s``s`kski", -- 115
 SLit $ LitStr "``s`k```sii``s``s`kski``s``s`ksk``s``s`ksk```sii``s``s`ksk``s``s`kski", -- 116
 SLit $ LitStr "`````sii``s``s`kski```s``s`ksk``s``s`ksk```sii``s``s`ksk``s``s`kski`s``s`kski", -- 117
 SLit $ LitStr "````s``s`kski````sii``s``s`ksk``s``s`kski`s``s`ksk```s`s``s`ksk``sii``s``s`kski", -- 118
 SLit $ LitStr "``s`k``s``s`ksk``s``s`ksk``s``s`ksk```sii``s``s`kski``s``s`ksk```s``siii``s``s`kski", -- 119
 SLit $ LitStr "``s`k```sii``s``s`kski``s``s`ksk``s``s`ksk``s``s`ksk```sii``s``s`ksk``s``s`kski", -- 120
 SLit $ LitStr "```s``s`kski````s``s`kski```s``s`ksk```sii``s``s`kski`s``s`kski", -- 121
 SLit $ LitStr "``s``s`ksk```s``s`kski````s``s`kski```s``s`ksk```sii``s``s`kski`s``s`kski", -- 122
 SLit $ LitStr "``s``s`ksk``s``s`ksk```s``s`kski````s``s`kski```s``s`ksk```sii``s``s`kski`s``s`kski", -- 123
 SLit $ LitStr "`````sii``s``s`kski````sii``s``s`ksk``s``s`kski`s``s`ksk```s``siii``s``s`kski", -- 124
 SLit $ LitStr "```s``s`ksk``s``s`kski``s``s`ksk```sii``s``s`kski", -- 125
 SLit $ LitStr "``s``s`ksk```s``s`ksk``s``s`kski``s``s`ksk```sii``s``s`kski", -- 126
 SLit $ LitStr "``s``s`ksk``s``s`ksk```s``s`ksk``s``s`kski``s``s`ksk```sii``s``s`kski", -- 127
 SLit $ LitStr "``s`k``s``s`kski```s`s``s`ksk``sii``s``s`kski", -- 128
 SLit $ LitStr "````s``s`kski````s`s``s`ksk``sii``s``s`kski`s``s`kski", -- 129
 SLit $ LitStr "``s`k``s``s`kski``s``s`ksk```s`s``s`ksk``sii``s``s`kski", -- 130
 SLit $ LitStr "````s``s`kski```s``s`ksk```s`s``s`ksk``sii``s``s`kski`s``s`kski", -- 131
 SLit $ LitStr "``s`k```sii``s``s`kski````s``s`kski````s``siii``s``s`kski`s``s`kski", -- 132
 SLit $ LitStr "`````sii``s``s`kski`````s``s`kski````s``siii``s``s`kski`s``s`kski`s``s`kski", -- 133
 SLit $ LitStr "``s`k``s``s`kski``s``s`ksk``s``s`ksk``s``s`ksk```s`s``s`ksk``sii``s``s`kski", -- 134
 SLit $ LitStr "``s`k``s``s`ksk```sii``s``s`kski```sii``s``s`ksk``s``s`kski", -- 135
 SLit $ LitStr "````s``s`ksk```sii``s``s`kski````sii``s``s`ksk``s``s`kski`s``s`kski", -- 136
 SLit $ LitStr "``s``s`ksk````s``s`ksk```sii``s``s`kski````sii``s``s`ksk``s``s`kski`s``s`kski", -- 137
 SLit $ LitStr "``s`k``s``s`kski`````sii``s``s`kski```s``s`ksk```s``siii``s``s`kski`s``s`kski", -- 138
 SLit $ LitStr "````s``s`ksk```sii``s``s`kski````sii``s``s`ksk``s``s`kski`s``s`ksk```sii``s``s`kski", -- 139
 SLit $ LitStr "``s`k``s``s`ksk```sii``s``s`kski``s``s`ksk```sii``s``s`ksk``s``s`kski", -- 140
 SLit $ LitStr "````s``s`ksk```sii``s``s`kski```s``s`ksk```sii``s``s`ksk``s``s`kski`s``s`kski", -- 141
 SLit $ LitStr "``s``s`ksk````s``s`ksk```sii``s``s`kski```s``s`ksk```sii``s``s`ksk``s``s`kski`s``s`kski", -- 142
 SLit $ LitStr "``s``s`ksk``s``s`ksk````s``s`ksk```sii``s``s`kski```s``s`ksk```sii``s``s`ksk``s``s`kski`s``s`kski", -- 143
 SLit $ LitStr "```s``s`kski``s`k``s``s`ksk``s``s`kski```sii``s``s`kski", -- 144
 SLit $ LitStr "``s``s`ksk```s``s`kski``s`k``s``s`ksk``s``s`kski```sii``s``s`kski", -- 145
 SLit $ LitStr "``s``s`ksk``s``s`ksk```s``s`kski``s`k``s``s`ksk``s``s`kski```sii``s``s`kski", -- 146
 SLit $ LitStr "``s`k``s``s`ksk``s``s`kski````s``s`ksk``s``s`kski````s``siii``s``s`kski`s``s`kski", -- 147
 SLit $ LitStr "``s`k```sii``s``s`kski``s``s`ksk```s``s`kski``s``s`ksk``s``s`ksk```sii``s``s`kski", -- 148
 SLit $ LitStr "`````sii``s``s`kski```s``s`ksk```s``siii``s``s`kski`s``s`ksk```s``sii`s``s`ksk``s``s`kski", -- 149
 SLit $ LitStr "``s`k``s``s`ksk``s``s`ksk```sii``s``s`kski```s``s`kski``s``s`ksk```sii``s``s`kski", -- 150
 SLit $ LitStr "````s``s`ksk```sii``s``s`kski````sii``s``s`ksk``s``s`kski`s``s`ksk```s``siii``s``s`kski", -- 151
 SLit $ LitStr "`````sii``s``s`ksk``s``s`kski`s``s`ksk```s``s`ksk``s``s`kski``s``s`ksk```sii``s``s`kski", -- 152
 SLit $ LitStr "``s`k```s``s`kski``s``s`ksk``s``s`kski``s``s`ksk```s``siii``s``s`kski", -- 153
 SLit $ LitStr "`````s``s`kski``s``s`ksk``s``s`kski```s``s`ksk```s``siii``s``s`kski`s``s`kski", -- 154
 SLit $ LitStr "````s``s`kski````s`s``s`ksk``sii``s``s`kski`s``s`ksk```sii``s``s`ksk``s``s`kski", -- 155
 SLit $ LitStr "``s``s`ksk````s``s`kski````s`s``s`ksk``sii``s``s`kski`s``s`ksk```sii``s``s`ksk``s``s`kski", -- 156
 SLit $ LitStr "````s``s`kski```s``s`ksk```s`s``s`ksk``sii``s``s`kski`s``s`ksk```sii``s``s`ksk``s``s`kski", -- 157
 SLit $ LitStr "``s`k``s``s`kski````s``s`ksk``s``s`kski```s``s`ksk```s``s`kski``s``s`ksk```sii``s``s`kski`s``s`kski", -- 158
 SLit $ LitStr "`````sii``s``s`kski`````s``s`kski````s``siii``s``s`kski`s``s`kski`s``s`ksk```sii``s``s`ksk``s``s`kski", -- 159
 SLit $ LitStr "``s`k``s``s`kski``s`k``s``s`ksk```sii``s``s`kski```s``siii``s``s`kski", -- 160
 SLit $ LitStr "````s``s`kski```s``s`ksk```sii``s``s`kski````s``siii``s``s`kski`s``s`kski", -- 161
 SLit $ LitStr "``s`k``s``s`kski```s``sii`s``s`ksk``s``s`kski", -- 162
 SLit $ LitStr "````s``s`kski````s``sii`s``s`ksk``s``s`kski`s``s`kski", -- 163
 SLit $ LitStr "``s`k``s``s`kski``s``s`ksk```s``sii`s``s`ksk``s``s`kski", -- 164
 SLit $ LitStr "````s``s`kski```s``s`ksk```s``sii`s``s`ksk``s``s`kski`s``s`kski", -- 165
 SLit $ LitStr "``s`k``s``s`kski``s``s`ksk``s``s`ksk```s``sii`s``s`ksk``s``s`kski", -- 166
 SLit $ LitStr "````s``s`kski```s``s`ksk``s``s`ksk```s``sii`s``s`ksk``s``s`kski`s``s`kski", -- 167
 SLit $ LitStr "``s`k``s``s`kski``s``s`ksk``s``s`ksk``s``s`ksk```s``sii`s``s`ksk``s``s`kski", -- 168
 SLit $ LitStr "```s``s`kski````s``s`ksk``s``s`kski````sii``s``s`kski`s``s`kski", -- 169
 SLit $ LitStr "``s``s`ksk```s``s`kski````s``s`ksk``s``s`kski````sii``s``s`kski`s``s`kski", -- 170
 SLit $ LitStr "``s``s`ksk``s``s`ksk```s``s`kski````s``s`ksk``s``s`kski````sii``s``s`kski`s``s`kski", -- 171
 SLit $ LitStr "``s`k```sii``s``s`kski`````s``siii``s``s`kski`s``s`ksk```sii``s``s`ksk``s``s`kski", -- 172
 SLit $ LitStr "`````sii``s``s`kski``````s``siii``s``s`kski`s``s`ksk```sii``s``s`ksk``s``s`kski`s``s`kski", -- 173
 SLit $ LitStr "``s`k``s``s`ksk``s``s`ksk```sii``s``s`kski``s``s`ksk``s``s`ksk```sii``s``s`ksk``s``s`kski", -- 174
 SLit $ LitStr "``s`k``s``s`ksk```sii``s``s`kski````s``s`kski```s``s`ksk```s``siii``s``s`kski`s``s`kski", -- 175
 SLit $ LitStr "``s`k````s``s`kski```s``s`ksk```sii``s``s`kski`s``s`kski```s``siii``s``s`kski", -- 176
 SLit $ LitStr "``````s``s`kski```s``s`ksk```sii``s``s`kski`s``s`kski````s``siii``s``s`kski`s``s`kski", -- 177
 SLit $ LitStr "````s``s`kski````s``sii`s``s`ksk``s``s`kski`s``s`ksk```s``siii``s``s`kski", -- 178
 SLit $ LitStr "``s``s`ksk````s``s`kski````s``sii`s``s`ksk``s``s`kski`s``s`ksk```s``siii``s``s`kski", -- 179
 SLit $ LitStr "``s`k``s``s`ksk```sii``s``s`kski```s``s`kski``s``s`ksk``s``s`ksk```sii``s``s`kski", -- 180
 SLit $ LitStr "````s``s`ksk```sii``s``s`kski````s``s`kski``s``s`ksk``s``s`ksk```sii``s``s`kski`s``s`kski", -- 181
 SLit $ LitStr "``s`k``s``s`kski`````sii``s``s`ksk``s``s`kski`s``s`ksk```s`s``s`ksk``sii``s``s`kski", -- 182
 SLit $ LitStr "````s``s`kski``````sii``s``s`ksk``s``s`kski`s``s`ksk```s`s``s`ksk``sii``s``s`kski`s``s`kski", -- 183
 SLit $ LitStr "``s`k``s``s`kski``s``s`ksk`````sii``s``s`ksk``s``s`kski`s``s`ksk```s`s``s`ksk``sii``s``s`kski", -- 184
 SLit $ LitStr "`````s``siii``s``s`kski`s``s`ksk```s``s`kski````s``s`ksk``s``s`kski````sii``s``s`kski`s``s`kski", -- 185
 SLit $ LitStr "``s`k``s``s`kski````s``s`ksk``s``s`kski````sii``s``s`kski`s``s`ksk```s``sii`s``s`ksk``s``s`kski", -- 186
 SLit $ LitStr "``s`k````s``s`kski```s``s`ksk```sii``s``s`kski`s``s`kski``s``s`ksk```s``siii``s``s`kski", -- 187
 SLit $ LitStr "``````s``s`kski```s``s`ksk```sii``s``s`kski`s``s`kski```s``s`ksk```s``siii``s``s`kski`s``s`kski", -- 188
 SLit $ LitStr "````s``s`kski````s``sii`s``s`ksk``s``s`kski`s``s`ksk```sii``s``s`ksk``s``s`kski", -- 189
 SLit $ LitStr "``s``s`ksk````s``s`kski````s``sii`s``s`ksk``s``s`kski`s``s`ksk```sii``s``s`ksk``s``s`kski", -- 190
 SLit $ LitStr "````s``s`kski```s``s`ksk```s``sii`s``s`ksk``s``s`kski`s``s`ksk```sii``s``s`ksk``s``s`kski", -- 191
 SLit $ LitStr "``s`k``s``s`ksk``s``s`kski```s`s``s`ksk``sii``s``s`kski", -- 192
 SLit $ LitStr "````s``s`ksk``s``s`kski````s`s``s`ksk``sii``s``s`kski`s``s`kski", -- 193
 SLit $ LitStr "``s``s`ksk````s``s`ksk``s``s`kski````s`s``s`ksk``sii``s``s`kski`s``s`kski", -- 194
 SLit $ LitStr "``s`k``s``s`ksk``s``s`kski``s``s`ksk```s`s``s`ksk``sii``s``s`kski", -- 195
 SLit $ LitStr "```s``s`kski``s``s`ksk````s``s`ksk``s``s`kski````sii``s``s`kski`s``s`kski", -- 196
 SLit $ LitStr "``s``s`ksk```s``s`kski``s``s`ksk````s``s`ksk``s``s`kski````sii``s``s`kski`s``s`kski", -- 197
 SLit $ LitStr "``s`k``s``s`ksk``s``s`kski``s``s`ksk``s``s`ksk```s`s``s`ksk``sii``s``s`kski", -- 198
 SLit $ LitStr "````s``s`ksk``s``s`kski```s``s`ksk``s``s`ksk```s`s``s`ksk``sii``s``s`kski`s``s`kski", -- 199
 SLit $ LitStr "``s`k``s``s`kski```s``s`kski``s`k``s``s`kski``s``s`ksk```sii``s``s`kski", -- 200
 SLit $ LitStr "````s``s`kski````s``s`kski``s`k``s``s`kski``s``s`ksk```sii``s``s`kski`s``s`kski", -- 201
 SLit $ LitStr "``s`k``s``s`kski``s``s`ksk```s``s`kski``s`k``s``s`kski``s``s`ksk```sii``s``s`kski", -- 202
 SLit $ LitStr "````s``s`kski```s``s`ksk```s``s`kski``s`k``s``s`kski``s``s`ksk```sii``s``s`kski`s``s`kski", -- 203
 SLit $ LitStr "``s`k``s``s`ksk``s``s`kski``s`k```sii``s``s`kski``s``s`ksk```s``siii``s``s`kski", -- 204
 SLit $ LitStr "````s``s`ksk``s``s`kski````sii``s``s`kski```s``s`ksk```s``siii``s``s`kski`s``s`kski", -- 205
 SLit $ LitStr "`````s``sii`s``s`ksk``s``s`kski`s``s`ksk```s``s`ksk``s``s`kski``s``s`ksk```sii``s``s`kski", -- 206
 SLit $ LitStr "``s`k``s``s`ksk``s``s`kski`````sii``s``s`kski```s``s`ksk```s``siii``s``s`kski`s``s`kski", -- 207
 SLit $ LitStr "``s`k````s``s`ksk``s``s`kski````sii``s``s`kski`s``s`kski```s``siii``s``s`kski", -- 208
 SLit $ LitStr "````s``s`kski````s`s``s`ksk``sii``s``s`kski`s``s`ksk```s``sii`s``s`ksk``s``s`kski", -- 209
 SLit $ LitStr "``s``s`ksk````s``s`kski````s`s``s`ksk``sii``s``s`kski`s``s`ksk```s``sii`s``s`ksk``s``s`kski", -- 210
 SLit $ LitStr "````s``s`kski```s``s`ksk```s`s``s`ksk``sii``s``s`kski`s``s`ksk```s``sii`s``s`ksk``s``s`kski", -- 211
 SLit $ LitStr "``s`k``s``s`kski`````s``s`kski``s``s`ksk```sii``s``s`kski`s``s`ksk```s``sii`s``s`ksk``s``s`kski", -- 212
 SLit $ LitStr "`````sii``s``s`kski`````s``s`kski````s``siii``s``s`kski`s``s`kski`s``s`ksk```s``sii`s``s`ksk``s``s`kski", -- 213
 SLit $ LitStr "``s`k``s``s`kski````s``s`ksk```sii``s``s`kski````s``siii``s``s`kski`s``s`ksk```sii``s``s`ksk``s``s`kski", -- 214
 SLit $ LitStr "``s`k``s``s`ksk```sii``s``s`kski`````s``siii``s``s`kski`s``s`ksk```sii``s``s`ksk``s``s`kski", -- 215
 SLit $ LitStr "```s``s`ksk``s``s`kski``s``s`ksk``s``s`ksk```sii``s``s`kski", -- 216
 SLit $ LitStr "``s``s`ksk```s``s`ksk``s``s`kski``s``s`ksk``s``s`ksk```sii``s``s`kski", -- 217
 SLit $ LitStr "``s`k``s``s`kski`````sii``s``s`kski````sii``s``s`ksk``s``s`kski`s``s`kski", -- 218
 SLit $ LitStr "````s``s`kski``````sii``s``s`kski````sii``s``s`ksk``s``s`kski`s``s`kski`s``s`kski", -- 219
 SLit $ LitStr "``s`k```sii``s``s`kski````s``s`kski````sii``s``s`ksk``s``s`kski`s``s`kski", -- 220
 SLit $ LitStr "`````sii``s``s`kski`````s``s`kski````sii``s``s`ksk``s``s`kski`s``s`kski`s``s`kski", -- 221
 SLit $ LitStr "``s``s`ksk`````sii``s``s`kski`````s``s`kski````sii``s``s`ksk``s``s`kski`s``s`kski`s``s`kski", -- 222
 SLit $ LitStr "``s``s`ksk``s``s`ksk`````sii``s``s`kski`````s``s`kski````sii``s``s`ksk``s``s`kski`s``s`kski`s``s`kski", -- 223
 SLit $ LitStr "``s`k``s``s`kski``s`k```sii``s``s`kski``s``s`ksk```sii``s``s`ksk``s``s`kski", -- 224
 SLit $ LitStr "```s``s`kski``s`k``s``s`ksk``s``s`kski``s``s`ksk```sii``s``s`kski", -- 225
 SLit $ LitStr "``s``s`ksk```s``s`kski``s`k``s``s`ksk``s``s`kski``s``s`ksk```sii``s``s`kski", -- 226
 SLit $ LitStr "``s``s`ksk``s``s`ksk```s``s`kski``s`k``s``s`ksk``s``s`kski``s``s`ksk```sii``s``s`kski", -- 227
 SLit $ LitStr "``s`k```sii``s``s`kski````s``s`kski```s``s`ksk```sii``s``s`ksk``s``s`kski`s``s`kski", -- 228
 SLit $ LitStr "`````sii``s``s`kski`````s``s`kski```s``s`ksk```sii``s``s`ksk``s``s`kski`s``s`kski`s``s`kski", -- 229
 SLit $ LitStr "``s``s`ksk`````sii``s``s`kski`````s``s`kski```s``s`ksk```sii``s``s`ksk``s``s`kski`s``s`kski`s``s`kski", -- 230
 SLit $ LitStr "``s`k``s``s`ksk``s``s`ksk``s``s`ksk```sii``s``s`kski````s``s`kski````s``siii``s``s`kski`s``s`kski", -- 231
 SLit $ LitStr "``s`k``s``s`kski``s`k```sii``s``s`kski``s``s`ksk``s``s`ksk```sii``s``s`ksk``s``s`kski", -- 232
 SLit $ LitStr "````s``s`kski````sii``s``s`kski```s``s`ksk``s``s`ksk```sii``s``s`ksk``s``s`kski`s``s`kski", -- 233
 SLit $ LitStr "``s`k```s``s`kski``s``s`ksk``s``s`kski``s``s`ksk```s``s`kski``s``s`ksk```sii``s``s`kski", -- 234
 SLit $ LitStr "`````s``s`kski``s``s`ksk``s``s`kski```s``s`ksk```s``s`kski``s``s`ksk```sii``s``s`kski`s``s`kski", -- 235
 SLit $ LitStr "``s`k```sii``s``s`kski````s``s`kski````s``siii``s``s`kski`s``s`ksk```sii``s``s`ksk``s``s`kski", -- 236
 SLit $ LitStr "`````sii``s``s`kski`````s``s`kski````s``siii``s``s`kski`s``s`ksk```sii``s``s`ksk``s``s`kski`s``s`kski", -- 237
 SLit $ LitStr "``s`k``s``s`ksk````s``s`ksk``s``s`kski````sii``s``s`kski`s``s`kski``s``s`ksk```s``siii``s``s`kski", -- 238
 SLit $ LitStr "````s``s`ksk````s``s`ksk``s``s`kski````sii``s``s`kski`s``s`kski```s``s`ksk```s``siii``s``s`kski`s``s`kski", -- 239
 SLit $ LitStr "``s`k``s``s`ksk``s``s`kski``s`k``s``s`ksk```sii``s``s`kski```s``siii``s``s`kski", -- 240
 SLit $ LitStr "````s``s`ksk``s``s`kski```s``s`ksk```sii``s``s`kski````s``siii``s``s`kski`s``s`kski", -- 241
 SLit $ LitStr "``s`k``s``s`kski```s``s`kski````s``s`kski```s``s`ksk```sii``s``s`kski`s``s`kski", -- 242
 SLit $ LitStr "```s``s`ksk```sii``s``s`kski``s``s`ksk``s``s`kski", -- 243
 SLit $ LitStr "``s``s`ksk```s``s`ksk```sii``s``s`kski``s``s`ksk``s``s`kski", -- 244
 SLit $ LitStr "``s``s`ksk``s``s`ksk```s``s`ksk```sii``s``s`kski``s``s`ksk``s``s`kski", -- 245
 SLit $ LitStr "``s`k``s``s`ksk``s``s`kski``s``s`ksk```s``sii`s``s`ksk``s``s`kski", -- 246
 SLit $ LitStr "````s``s`ksk``s``s`kski```s``s`ksk```s``sii`s``s`ksk``s``s`kski`s``s`kski", -- 247
 SLit $ LitStr "``s``s`ksk````s``s`ksk``s``s`kski```s``s`ksk```s``sii`s``s`ksk``s``s`kski`s``s`kski", -- 248
 SLit $ LitStr "``s`k``s``s`ksk``s``s`kski``s``s`ksk``s``s`ksk```s``sii`s``s`ksk``s``s`kski", -- 249
 SLit $ LitStr "``s`k``s``s`kski```s``s`ksk``s``s`kski``s``s`ksk```sii``s``s`kski", -- 250
 SLit $ LitStr "````s``s`kski````s``s`ksk``s``s`kski``s``s`ksk```sii``s``s`kski`s``s`kski", -- 251
 SLit $ LitStr "``s`k``s``s`kski``s``s`ksk```s``s`ksk``s``s`kski``s``s`ksk```sii``s``s`kski", -- 252
 SLit $ LitStr "````s``s`kski```s``s`ksk```s``s`ksk``s``s`kski``s``s`ksk```sii``s``s`kski`s``s`kski", -- 253
 SLit $ LitStr "``s`k``s``s`kski``s``s`ksk``s``s`ksk```s``s`ksk``s``s`kski``s``s`ksk```sii``s``s`kski", -- 254
 SLit $ LitStr "``s`k``s``s`ksk``s``s`kski`````sii``s``s`kski`s``s`ksk```s``sii`s``s`ksk``s``s`kski", -- 255
 SLit $ LitStr "```sii```sii``s``s`kski"] -- 256


builtins = [
 ("+", SLit $ LitStr "``si`k`s``s`ksk"),
 ("-", SLit $ LitStr "``s`k`s`k```sii``s``s`ks``s`k`s``si`k`kk``s`k`s`k`s``s`ksk``s``s`ks``s`kk``sii`k``s``s``si`k`kk``si`kii`k`k`ki``s`k`s``si`k``s``s``si`k`kk``si`kii``s`kk``s``si`k``s`k`sik`k`k`ki"),
 ("*", SLit $ LitStr "``s`ksk"),
 ("div", SLit $ LitStr "``s`k`s```ss`s``s`ks``s`kk``s`k`s``s`ks``s`kk``s``s`ks``s`k`s``si`k`kk`s`k`s``s`ksk`k`k`ki``s`kk``si`k``s``s``si`k`kk``si`kii`k``sii``s`kk``s`k`si``s`kk``s``si`k``s`k`sik`k`k`ki"),
 ("mod", SLit $ LitStr "``s`k`s```ss`s``s`ks``s`kk``s`k`s``s`ks``s``s`ks``s`kk``s`ks`s``si`k`kk`k``s`kk``s`k```sii``s``s`ks``s`k`s``si`k`kk``s`k`s`k`s``s`ksk``s``s`ks``s`kk``sii`k``s``s``si`k`kk``si`kii`k`k`ki``s``s``si`k`kk``si`kii``s`kk``si`k``s``s``si`k`kk``si`kii`k``sii``s`kk``s`k`si``s`kk``s``si`k``s`k`sik`k`k`ki"),
 ("&eq", SLit $ LitStr "``s`k`s`k``si`kk``s`k`s``si`k``si`k`ki``s`kk``s``si`k``s`k`s``si`k`kik`k``s``si`kk`k```sii``s`k``s`k`s``si`k`kik``sii"),
 ("&neq", SLit $ LitStr "``s`k`s`k``si`kk``s`k`s``si`k``si`k`ki``s`kk``s``si`k``s`k`s``si`kkk`k``s``si`k`ki`k```sii``s`k``s`k`s``si`kkk``sii"),
 ("<=", SLit $ LitStr "``s``s`ks``s`kk``s``si`k``s`k`sik`k`kk`k``s``si`k``s`k`sik`k`k`ki"),
 (">=", SLit $ LitStr "``s`k`s``s``si`k``s`k`sik`k`kk``s`kk``s``si`k``s`k`sik`k`k`ki"),
 ("<", SLit $ LitStr "``s`k`s``s``si`k``s`k`sik`k`k`ki``s`kk``s``si`k``s`k`sik`k`kk"),
 (">", SLit $ LitStr "``s``s`ks``s`kk``s``si`k``s`k`sik`k`k`ki`k``s``si`k``s`k`sik`k`kk"),
 ("&&", SLit $ LitStr "``ss`k`k`ki"),
 ("||", SLit $ LitStr "``si`kk"),
 (".", SLit $ LitStr "``s`ksk"),
 ("++", SLit $ LitStr "```sii``s``s`ks``s`k`s`ks``s`k`s``s`ksk``s`kk``s`k`s`k`s``s`ks``s`kk``s`k`s`k`s`kk``s``s`ks``s`kk``s`ks``s`k`sik`kk``s`k`s`kk``s``s`ks``s`kk``s`ks``sii`kk`k`ki"),
 ("Y", SLit $ LitStr "```ss`s``s`ksk`k``sii"),
 ("U", SLit $ LitStr "``s``s`ks``s``s`ksk`k``si`kk`k``si`k`ki"),
 ("cons", SLit $ LitStr "``s``s`ks``s`kk``s`ks``s`k`sik`kk"),
 ("nil", SLit $ LitStr "`kk"),
 ("IF", SVar "I"),
 ("ord", SVar "I"),
 ("chr", SVar "I")]

skiError = SVar "I"

expandBltin :: SKI -> SKI
expandBltin (SAp (SVar "error") _) = skiError
expandBltin (SAp e1 e2) = SAp (expandBltin e1) (expandBltin e2)
expandBltin (SVar v) = case lookup v builtins of
                         Just e -> e
                         Nothing -> (SVar v)
expandBltin (SLit (LitInt n)) = churchnums !! n
expandBltin (SLit (LitChar (c:_))) = churchnums !! ord c
