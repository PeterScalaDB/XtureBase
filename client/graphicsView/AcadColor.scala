/**
 * Author: Peter Started:09.10.2010
 */
package client.graphicsView

import java.awt.Color

/** color table for AUTOCAD DXF files
 * 
 */


object AcadColor {
	def convertColor(r:Int,g:Int,b:Int) = {
		((r & 0xFF) << 16) |
		((g & 0xFF) << 8)  |
		((b & 0xFF) << 0)  
	}
	
	val defaultColor= convertColor(0,0,0)
	val whiteColor=defaultColor
	
	
	
	val table=collection.immutable.HashMap[Int,Int](			
			(0 -> convertColor(0,0,0)),
			(1 -> convertColor(255,0,0)),
			(2 -> convertColor(255,255,0)),
			(3 -> convertColor(0,255,0)),
			(4 -> convertColor(0,255,255)),
			(5 -> convertColor(0,0,255)),
			(6 -> convertColor(255,0,255)),
			(7 -> whiteColor),
			(8 -> convertColor(65,65,65)),
			(9 -> convertColor(128,128,128)),
			(10 -> convertColor(255,0,0)),
			(11 -> convertColor(255,170,170)),
			(12 -> convertColor(189,0,0)),
			(13 -> convertColor(189,126,126)),
			(14 -> convertColor(129,0,0)),
			(15 -> convertColor(129,86,86)),
			(16 -> convertColor(104,0,0)),
			(17 -> convertColor(104,69,69)),
			(18 -> convertColor(79,0,0)),
			(19 -> convertColor(79,53,53)),
			(20 -> convertColor(255,63,0)),
			(21 -> convertColor(255,191,170)),
			(22 -> convertColor(189,46,0)),
			(23 -> convertColor(189,141,126)),
			(24 -> convertColor(129,31,0)),
			(25 -> convertColor(129,96,86)),
			(26 -> convertColor(104,25,0)),
			(27 -> convertColor(104,78,69)),
			(28 -> convertColor(79,19,0)),
			(29 -> convertColor(79,59,53)),
			(30 -> convertColor(255,127,0)),
			(31 -> convertColor(255,212,170)),
			(32 -> convertColor(189,94,0)),
			(33 -> convertColor(189,157,126)),
			(34 -> convertColor(129,64,0)),
			(35 -> convertColor(129,107,86)),
			(36 -> convertColor(104,52,0)),
			(37 -> convertColor(104,86,69)),
			(38 -> convertColor(79,39,0)),
			(39 -> convertColor(79,66,53)),
			(40 -> convertColor(255,191,0)),
			(41 -> convertColor(255,234,170)),
			(42 -> convertColor(189,141,0)),
			(43 -> convertColor(189,173,126)),
			(44 -> convertColor(129,96,0)),
			(45 -> convertColor(129,118,86)),
			(46 -> convertColor(104,78,0)),
			(47 -> convertColor(104,95,69)),
			(48 -> convertColor(79,59,0)),
			(49 -> convertColor(79,73,53)),
			(50 -> convertColor(255,255,0)),
			(51 -> convertColor(255,255,170)),
			(52 -> convertColor(189,189,0)),
			(53 -> convertColor(189,189,126)),
			(54 -> convertColor(129,129,0)),
			(55 -> convertColor(129,129,86)),
			(56 -> convertColor(104,104,0)),
			(57 -> convertColor(104,104,69)),
			(58 -> convertColor(79,79,0)),
			(59 -> convertColor(79,79,53)),
			(60 -> convertColor(191,255,0)),
			(61 -> convertColor(234,255,170)),
			(62 -> convertColor(141,189,0)),
			(63 -> convertColor(173,189,126)),
			(64 -> convertColor(96,129,0)),
			(65 -> convertColor(118,129,86)),
			(66 -> convertColor(78,104,0)),
			(67 -> convertColor(95,104,69)),
			(68 -> convertColor(59,79,0)),
			(69 -> convertColor(73,79,53)),
			(70 -> convertColor(127,255,0)),
			(71 -> convertColor(212,255,170)),
			(72 -> convertColor(94,189,0)),
			(73 -> convertColor(157,189,126)),
			(74 -> convertColor(64,129,0)),
			(75 -> convertColor(107,129,86)),
			(76 -> convertColor(52,104,0)),
			(77 -> convertColor(86,104,69)),
			(78 -> convertColor(39,79,0)),
			(79 -> convertColor(66,79,53)),
			(80 -> convertColor(63,255,0)),
			(81 -> convertColor(191,255,170)),
			(82 -> convertColor(46,189,0)),
			(83 -> convertColor(141,189,126)),
			(84 -> convertColor(31,129,0)),
			(85 -> convertColor(96,129,86)),
			(86 -> convertColor(25,104,0)),
			(87 -> convertColor(78,104,69)),
			(88 -> convertColor(19,79,0)),
			(89 -> convertColor(59,79,53)),
			(90 -> convertColor(0,255,0)),
			(91 -> convertColor(170,255,170)),
			(92 -> convertColor(0,189,0)),
			(93 -> convertColor(126,189,126)),
			(94 -> convertColor(0,129,0)),
			(95 -> convertColor(86,129,86)),
			(96 -> convertColor(0,104,0)),
			(97 -> convertColor(69,104,69)),
			(98 -> convertColor(0,79,0)),
			(99 -> convertColor(53,79,53)),
			(100 -> convertColor(0,255,63)),
			(101 -> convertColor(170,255,191)),
			(102 -> convertColor(0,189,46)),
			(103 -> convertColor(126,189,141)),
			(104 -> convertColor(0,129,31)),
			(105 -> convertColor(86,129,96)),
			(106 -> convertColor(0,104,25)),
			(107 -> convertColor(69,104,78)),
			(108 -> convertColor(0,79,19)),
			(109 -> convertColor(53,79,59)),
			(110 -> convertColor(0,255,127)),
			(111 -> convertColor(170,255,212)),
			(112 -> convertColor(0,189,94)),
			(113 -> convertColor(126,189,157)),
			(114 -> convertColor(0,129,64)),
			(115 -> convertColor(86,129,107)),
			(116 -> convertColor(0,104,52)),
			(117 -> convertColor(69,104,86)),
			(118 -> convertColor(0,79,39)),
			(119 -> convertColor(53,79,66)),
			(120 -> convertColor(0,255,191)),
			(121 -> convertColor(170,255,234)),
			(122 -> convertColor(0,189,141)),
			(123 -> convertColor(126,189,173)),
			(124 -> convertColor(0,129,96)),
			(125 -> convertColor(86,129,118)),
			(126 -> convertColor(0,104,78)),
			(127 -> convertColor(69,104,95)),
			(128 -> convertColor(0,79,59)),
			(129 -> convertColor(53,79,73)),
			(130 -> convertColor(0,255,255)),
			(131 -> convertColor(170,255,255)),
			(132 -> convertColor(0,189,189)),
			(133 -> convertColor(126,189,189)),
			(134 -> convertColor(0,129,129)),
			(135 -> convertColor(86,129,129)),
			(136 -> convertColor(0,104,104)),
			(137 -> convertColor(69,104,104)),
			(138 -> convertColor(0,79,79)),
			(139 -> convertColor(53,79,79)),
			(140 -> convertColor(0,191,255)),
			(141 -> convertColor(170,234,255)),
			(142 -> convertColor(0,141,189)),
			(143 -> convertColor(126,173,189)),
			(144 -> convertColor(0,96,129)),
			(145 -> convertColor(86,118,129)),
			(146 -> convertColor(0,78,104)),
			(147 -> convertColor(69,95,104)),
			(148 -> convertColor(0,59,79)),
			(149 -> convertColor(53,73,79)),
			(150 -> convertColor(0,127,255)),
			(151 -> convertColor(170,212,255)),
			(152 -> convertColor(0,94,189)),
			(153 -> convertColor(126,157,189)),
			(154 -> convertColor(0,64,129)),
			(155 -> convertColor(86,107,129)),
			(156 -> convertColor(0,52,104)),
			(157 -> convertColor(69,86,104)),
			(158 -> convertColor(0,39,79)),
			(159 -> convertColor(53,66,79)),
			(160 -> convertColor(0,63,255)),
			(161 -> convertColor(170,191,255)),
			(162 -> convertColor(0,46,189)),
			(163 -> convertColor(126,141,189)),
			(164 -> convertColor(0,31,129)),
			(165 -> convertColor(86,96,129)),
			(166 -> convertColor(0,25,104)),
			(167 -> convertColor(69,78,104)),
			(168 -> convertColor(0,19,79)),
			(169 -> convertColor(53,59,79)),
			(170 -> convertColor(0,0,255)),
			(171 -> convertColor(170,170,255)),
			(172 -> convertColor(0,0,189)),
			(173 -> convertColor(126,126,189)),
			(174 -> convertColor(0,0,129)),
			(175 -> convertColor(86,86,129)),
			(176 -> convertColor(0,0,104)),
			(177 -> convertColor(69,69,104)),
			(178 -> convertColor(0,0,79)),
			(179 -> convertColor(53,53,79)),
			(180 -> convertColor(63,0,255)),
			(181 -> convertColor(191,170,255)),
			(182 -> convertColor(46,0,189)),
			(183 -> convertColor(141,126,189)),
			(184 -> convertColor(31,0,129)),
			(185 -> convertColor(96,86,129)),
			(186 -> convertColor(25,0,104)),
			(187 -> convertColor(78,69,104)),
			(188 -> convertColor(19,0,79)),
			(189 -> convertColor(59,53,79)),
			(190 -> convertColor(127,0,255)),
			(191 -> convertColor(212,170,255)),
			(192 -> convertColor(94,0,189)),
			(193 -> convertColor(157,126,189)),
			(194 -> convertColor(64,0,129)),
			(195 -> convertColor(107,86,129)),
			(196 -> convertColor(52,0,104)),
			(197 -> convertColor(86,69,104)),
			(198 -> convertColor(39,0,79)),
			(199 -> convertColor(66,53,79)),
			(200 -> convertColor(191,0,255)),
			(201 -> convertColor(234,170,255)),
			(202 -> convertColor(141,0,189)),
			(203 -> convertColor(173,126,189)),
			(204 -> convertColor(96,0,129)),
			(205 -> convertColor(118,86,129)),
			(206 -> convertColor(78,0,104)),
			(207 -> convertColor(95,69,104)),
			(208 -> convertColor(59,0,79)),
			(209 -> convertColor(73,53,79)),
			(210 -> convertColor(255,0,255)),
			(211 -> convertColor(255,170,255)),
			(212 -> convertColor(189,0,189)),
			(213 -> convertColor(189,126,189)),
			(214 -> convertColor(129,0,129)),
			(215 -> convertColor(129,86,129)),
			(216 -> convertColor(104,0,104)),
			(217 -> convertColor(104,69,104)),
			(218 -> convertColor(79,0,79)),
			(219 -> convertColor(79,53,79)),
			(220 -> convertColor(255,0,191)),
			(221 -> convertColor(255,170,234)),
			(222 -> convertColor(189,0,141)),
			(223 -> convertColor(189,126,173)),
			(224 -> convertColor(129,0,96)),
			(225 -> convertColor(129,86,118)),
			(226 -> convertColor(104,0,78)),
			(227 -> convertColor(104,69,95)),
			(228 -> convertColor(79,0,59)),
			(229 -> convertColor(79,53,73)),
			(230 -> convertColor(255,0,127)),
			(231 -> convertColor(255,170,212)),
			(232 -> convertColor(189,0,94)),
			(233 -> convertColor(189,126,157)),
			(234 -> convertColor(129,0,64)),
			(235 -> convertColor(129,86,107)),
			(236 -> convertColor(104,0,52)),
			(237 -> convertColor(104,69,86)),
			(238 -> convertColor(79,0,39)),
			(239 -> convertColor(79,53,66)),
			(240 -> convertColor(255,0,63)),
			(241 -> convertColor(255,170,191)),
			(242 -> convertColor(189,0,46)),
			(243 -> convertColor(189,126,141)),
			(244 -> convertColor(129,0,31)),
			(245 -> convertColor(129,86,96)),
			(246 -> convertColor(104,0,25)),
			(247 -> convertColor(104,69,78)),
			(248 -> convertColor(79,0,19)),
			(249 -> convertColor(79,53,59)),
			(250 -> convertColor(51,51,51)),
			(251 -> convertColor(80,80,80)),
			(252 -> convertColor(105,105,105)),
			(253 -> convertColor(130,130,130)),
			(254 -> convertColor(190,190,190)),
			(255 -> defaultColor)	// defaultcolor
	)
}

