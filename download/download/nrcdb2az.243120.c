/*
################################################################################
@(#) read an NRC DataBank file in PIB format and convert to ASCII zebra format

This program reads an NRC DataBank file in PIB format and converts it
to a file format (ASCII zebra) that can be read by USH. It is based
on the  following document:

   PIB File Specification  - K. R. Jones, April 1997

                       Scientech Inc.
              11140 Rockville Pike * Suite 500
                    Rockville, MD 20852
           Phone 301/468-6425 * Fax 301/468-0883

The file structure is reminiscent of a USH file, in that there is a
title section (called the File Header Block) followed by a header section
(called the Channel Header Block), followed by a Data section. But the
data is not multiplexed; it is in stripes like a ZEBRA file. In addition,
the data may be compressed. The data is written using RCP xdr routines
for machine portability.

A.       File Header Block

The PIB file begins with a header block that is used to identify the
file type and to provide the information necessary to
read the subsequent channel header records.  It may also include an
optional list of filenames that were used to create the PIB file along
with an integer value indicating their file type. Currently, integer
values of 1000 and 2000 are reserved to identify binary and PIB file
types respectively.  This file list can prove useful in tracking down data
sources, especially if several files are combined using the merge utility.

Table 1 illustrates the layout of the file header block.

 Table 1. File Header Block Layout
   XDR                    Description                 Field Contents
 Primitive
  xdr_string  File type string                        "NRCDB V2.0, K. R. Jones"
  xdr_int     Header size (currently not used) 0
  xdr_int     Number of data channels                 numOfChnls
  xdr_int     Number of filenames (0 to 80)           numOfFiles
  xdr_string  Name of 1st file.  (optional)           fromfile[0]
  xdr_string  Name of 2nd file.  (optional)           fromfile[1]
  ... ...                                             ...
  xdr_string  Name of last file. (optional)           fromfile[numOfFiles]
  xdr_int     Type of 1st file.  (optional)           fromfileType[0]
  xdr_int     Type of 2nd file.  (optional)           fromfileType[1]
  ... ...                                             ...
  xdr_int     Type of last file. (optional)           fromfileType[numOfFiles]
  xdr_string  Name of file being created              filename

The following xdr_fileHead routine can be used to read or write the file
header block to an existing XDR stream.  The header data is retrieved
from or placed into a pibHeader structure as defined below:
*/

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
/*----------------------------------------------------------*/
#ifdef RPC_STD
#include <rpc/rpc.h>
#endif

#ifdef RPC_XDR_XX
/* moved from rpc.h on CygWin */
/* #include <rpc/xdr.h> */
#endif

/*----------------------------------------------------------*/
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

int VERBOSE;

/*----------------------------------------------------------
These are taken from the PDF document in this directory,
and are suspect because of a large number of repeats and
some obvious typographical errors, some of which were
corrected.

Accumulated was spelled wrong
RAYLEIGH, not RALEIGH
----------------------------------------------------------*/
#define NKEYS ( sizeof eunits / sizeof eunits[0])
struct Converts {
   int eunit;
   int ushunit;
   char *label_str;
   char *unit_str;
} eunits[]={
-1	,	-1	,	"UNMATCHED"			,	" "	,
321	,	1147	,	"AYA Liquid Velocity"		,	"m/s"	,
322	,	1147	,	"AYA Vapor Velocity"		,	"m/s"	,
358	,	1120	,	"Absolute Pressure"		,	"kg/m*s^2"	,
59	,	  1	,	"Acceleration"			,	"ft/s^2"	,
264	,	1178	,	"Accumulated CP Seconds"	,	"s"	,
265	,	1178	,	"Accumulated I/O Seconds"	,	"s"	,
393	,	  4	,	"Alkalinity (as CaCO3)"		,	"mg/kg"	,
89	,	  5	,	"Angular Velocity"		,	"rad/s"	,
195	,	  8	,	"Area"				,	"cm^2"	,
192	,	  7	,	"Area"				,	"ft^2"	,
194	,	  6	,	"Area"				,	"in^2"	,
193	,	  9	,	"Area"				,	"m^2"	,
294	,	1157	,	"Ave Momentum Flux"		,	"mg/m*s^2"	,
421	,	1122	,	"Average Density"		,	"kg/m^3"	,
289	,	1201	,	"Average Density"		,	"mg/m^3"	,
341	,	1201	,	"Average Density"		,	"mg/m^3"	,
425	,	1123	,	"Average Mass Flow Rate"	,	"kg/s"	,
290	,	1040	,	"Average Pressure"		,	"MPa"	,
291	,	1105	,	"Average Pressure"		,	"kPa"	,
292	,	1035	,	"Average Temperature"		,	"K"	,
98	,	1076	,	"Average Velocity"		,	"ft/s"	,
293	,	1147	,	"Average Velocity"		,	"m/s"	,
218	,	1000	,	"BOILING NUMBER"		,	" "	,
422	,	1122	,	"Beam Density"			,	"kg/m^3"	,
350	,	10	,	"Boron Concentration"		,	"ppm"	,
253	,	1178	,	"CP SECONDS ELEPHANT JOB CLASS"	,	"s"	,
252	,	1178	,	"CP SECONDS LARGE JOB CLASS"	,	"s"	,
251	,	1178	,	"CP SECONDS MEDIUM JOB CLASS"	,	"s"	,
250	,	1178	,	"CP SECONDS SMALL JOB CLASS"	,	"s"	,
394	,	1093	,	"Calculated Diff Pressure"	,	"in h2o"	,
50	,	15	,	"Choking Index"			,	" "	,
377	,	1004	,	"Cladding Axial Strain"		,	"%"	,
380	,	1004	,	"Cladding Circ Strain"		,	"%"	,
122	,	1000	,	"Cladding Elongation"		,	" "	,
121	,	1160	,	"Cladding Elongation"		,	"mm"	,
382	,	1025	,	"Cladding Surface Temp"		,	"C"	,
125	,	1035	,	"Cladding Surface Temperature"	,	"K"	,
330	,	1026	,	"Cladding Temperature"		,	"F"	,
274	,	1035	,	"Cladding Temperature"		,	"K"	,
389	,	24	,	"Concentration"			,	"mg/kg" 	,
390	,	23	,	"Concentration"			,	"ppm" 	,
391	,	1205	,	"Conductivity"			,	"mu*mno/cm"	,
329	,	1026	,	"Coolant Temperature"		,	"F"	,
278	,	1035	,	"Coolant Temperature"		,	"K"	,
1	,	1026	,	"Core Heater Temperature"	,	"F"	,
60	,	1035	,	"Core Heater Temperature"	,	"K"	,
231	,	1198	,	"Counts"			,	"log[c/s]"	,
179	,	31	,	"Crit. Heat Flux"		,	"btu/hr*ft^2"	,
180	,	33	,	"Crit. Heat Flux"		,	"kW/m^2"	,
154	,	32	,	"Critical Heat Flux"		,	"W/m^2"	,
11	,	45	,	"Current"			,	"Amp"	,
230	,	1190	,	"Current"			,	"amp"	,
272	,	1195	,	"Current"			,	"ka"	,
312	,	1197	,	"Current"			,	"log[A]"	,
299	,	44	,	"Current"			,	"mA"	,
13	,	46	,	"Decibels"			,	"dB"	,
238	,	1211	,	"Delta-Theta"			,	"rad"	,
423	,	1052	,	"Densitometer Output"		,	"V"	,
409	,	49	,	"Density"			,	"kg/m^3"	,
71	,	49	,	"Density"			,	"kg/m^3"	,
17	,	47	,	"Density"			,	"lbm/ft^3"	,
232	,	1201	,	"Density"			,	"mg/m^3"	,
247	,	51	,	"Depressurization Rate"		,	"kPa/s"	,
199	,	62	,	"Diameter"			,	"cm"	,
196	,	65	,	"Diameter"			,	"ft"	,
198	,	60	,	"Diameter"			,	"in"	,
197	,	63	,	"Diameter"			,	"m"	,
426	,	1105	,	"Diff. Pressure-Liquid Level"	,	"KPa"	,
273	,	76	,	"Differential Pressure"		,	"MPa"	,
284	,	74	,	"Differential Pressure"		,	"Pa"	,
363	,	72	,	"Differential Pressure"		,	"bar"	,
332	,	1091	,	"Differential Pressure"		,	"in"	,
70	,	75	,	"Differential Pressure"		,	"kPa"	,
359	,	1120	,	"Differential Pressure"		,	"kg/m*s^2"	,
385	,	73	,	"Differential Pressure"		,	"m-h2o"	,
366	,	1155	,	"Differential Pressure"		,	"mb"	,
348	,	76	,	"Differential Pressure"		,	"mmwg"	,
16	,	1210	,	"Differential Pressure"		,	"psid"	,
206	,	80	,	"Discharge Coefficient"		,	" "	,
316	,	84	,	"Displacement"			,	"cm"	,
311	,	81	,	"Displacement"			,	"in"	,
318	,	81	,	"Displacement"			,	"in"	,
314	,	85	,	"Displacement"			,	"m"	,
280	,	83	,	"Displacement"			,	"mm"	,
190	,	87	,	"Distance"			,	"ft"	,
191	,	89	,	"Distance"			,	"m"	,
405	,	89	,	"Distance"			,	"m"	,
307	,	1158	,	"Distance"			,	"mil"	,
302	,	1160	,	"Distance"			,	"mm"	,
412	,	1160	,	"Distance"			,	"mm"	,
402	,	1215	,	"Distance"			,	"um"	,
266	,	1206	,	"Drag Disk"			,	"mv"	,
429	,	1052	,	"Drag Disk Output"		,	"V"	,
223	,	91     	,	"ECKERT NUMBER"		,	" "	,
367	,	110	,	"EDQ"				,	" "	,
224	,	110	,	"EULER NUMBER"			,	" "	,
30	,	93	,	"Elevation"			,	"ft"	,
83	,	95	,	"Elevation"			,	"m"	,
40	,	99	,	"Energy"			,	"Btu"	,
305	,	1033	,	"Energy"			,	"J/kg"	,
335	,	101	,	"Energy"			,	"MW*hr"	,
23	,	106	,	"Enthalpy"			,	"Btu/lbm"	,
243	,	1185	,	"Enthalpy"			,	"GJ"	,
76	,	1033	,	"Enthalpy"			,	"J/kg"	,
244	,	1042	,	"Enthalpy Flow"			,	"MW"	,
404	,	1107	,	"Enthalpy Flow"			,	"kW"	,
361	,	1000	,	"Event"				,	" "	,
212	,	128	,	"FROUDE NUMBER"			,	" "	,
337	,	1192	,	"Fission Product Detectors"	,	"counts"	,
184	,	113	,	"Flooding Rate"			,	"ft/s"	,
185	,	115	,	"Flooding Rate"			,	"m/s"	,
104	,	116	,	"Flow Quality"			,	" "	,
387	,	1118	,	"Flow Rate"			,	"kg/hr"	,
207	,	117	,	"Flow Regime"			,	" "	,
277	,	1201	,	"Fluid Density"			,	"mg/m^3"	,
342	,	1201	,	"Fluid Density"			,	"mg/m^3"	,
428	,	1117	,	"Fluid Mass in Component"	,	"kg"	,
331	,	1026	,	"Fluid Subcooling"		,	"F"	,
283	,	1035	,	"Fluid Subcooling"		,	"K"	,
2	,	1026	,	"Fluid Temperature"		,	"F"	,
61	,	1035	,	"Fluid Temperature"		,	"K"	,
82	,	1063	,	"Fluid Velocity"		,	"cm/s"	,
28	,	1076	,	"Fluid Velocity"		,	"ft/s"	,
6	,	1076	,	"Fluid Velocity"		,	"ft/s"	,
65	,	1147	,	"Fluid Velocity"		,	"m/s"	,
66	,	122	,	"Force"				,	"N"	,
7	,	1130	,	"Force"				,	"lb"	,
400	,	124	,	"Frequency"			,	"Hz"	,
57	,	124	,	"Frequency"			,	"Hz"	,
208	,	126	,	"Friction Factor"		,	" "	,
376	,	1004	,	"Fuel Axial Strain"		,	"%"	,
379	,	1025	,	"Fuel Centerline Temperature"	,	"C"	,
118	,	1035	,	"Fuel Centerline Temperature"	,	"K"	,
117	,	1035	,	"Fuel Off-Center Temperature"	,	"K"	,
369	,	1035	,	"Fuel Plenum Temperature"	,	"K"	,
114	,	1112	,	"Fuel Rod Ave Power"		,	"kW/m"	,
301	,	1111	,	"Fuel Rod Average Power"	,	"kW/ft"	,
113	,	1112	,	"Fuel Rod Peak Power"		,	"kW/m"	,
111	,	1026	,	"Fuel Rod Temperature"		,	"F"	,
370	,	1035	,	"Fuel Temperature"		,	"K"	,
355	,	1000	,	"G's"				,	" "	,
353	,	1000	,	"G's/Radian"			,	" "	,
220	,	138	,	"GRASHOF NUMBER"		,	" "	,
381	,	135	,	"Gap Conductance"		,	"Btu/hr*ft^2*F"	,
308	,	1194	,	"Gas Flow Rate"			,	"gm*moles/s"	,
110	,	1026	,	"Guide Tube Temperature"	,	"F"	,
279	,	1035	,	"Guide Tube Temperature"	,	"K"	,
20	,	147	,	"H. T. Coeff."			,	"Btu/s*ft^2*F"	,
73	,	149	,	"H. T. Coeff."			,	"W/m^2*K"	,
339	,	148	,	"H. T. Coeff."			,	"btu/s*ft^2*F"	,
436	,	1214	,	"HTC Probe Output"		,	"uV"	,
403	,	1187	,	"Heat"				,	"Nm"	,
19	,	143	,	"Heat Flux"			,	"Btu/s*ft^2"	,
388	,	145	,	"Heat Flux"			,	"W/m^2"	,
72	,	145	,	"Heat Flux"			,	"W/m^2"	,
338	,	143	,	"Heat Flux"			,	"btu/s*ft^2"	,
155	,	149	,	"Heat Transfer Coef"		,	"W/m^2*K"	,
51	,	1000	,	"Heat Transfer Mode"		,	" "	,
45	,	152	,	"Heat Transfer Rate"		,	"Btu/s"	,
427	,	1042	,	"Heat Transfer Rate"		,	"MW"	,
153	,	153	,	"Heat Transfer Rate"		,	"W"	,
172	,	156	,	"Heat of Vaporization"		,	"Btu/lbm"	,
173	,	157	,	"Heat of Vaporization"		,	"kJ/kg"	,
435	,	1035	,	"Heated Thermocouple Output"	,	"K"	,
101	,	1107	,	"Horsepower"			,	"kW"	,
257	,	1178	,	"I/O SECONDS ELEPHANT JOB CLASS"	,	"s"	,
256	,	1178	,	"I/O SECONDS LARGE JOB CLASS"	,	"s"	,
255	,	1178	,	"I/O SECONDS MEDIUM JOB CLASS"	,	"s"	,
254	,	1178	,	"I/O SECONDS SMALL JOB CLASS"	,	"s"	,
262	,	1178	,	"INTERCOM I/O TIME"		,	"s"	,
261	,	1178	,	"INTERCON CP TIME"		,	"s"	,
368	,	1000	,	"IQF"				,	" "	,
344	,	1016	,	"Inlet Subcooling"		,	"btu/lbm"	,
120	,	1035	,	"Inlet Temperature"		,	"K"	,
440	,	1117	,	"Int. Catch Tank Disc. Flow"	,	"kg"	,
80	,	166	,	"Integrated Mass Flow"		,	"kg"	,
26	,	165	,	"Integrated Mass Flow"		,	"lbm"	,
417	,	1178	,	"Intercom I/O Time"		,	"sec"	,
147	,	174	,	"Interface Velocity"		,	"m/s"	,
54	,	1026	,	"Internal Rod Temperature"	,	"F"	,
93	,	1035	,	"Internal Rod Temperature"	,	"K"	,
260	,	175	,	"JULIAN DAY"			,	" "	,
150	,	1033	,	"Junction L/I Energy"		,	"J/kg"	,
148	,	1122	,	"Junction Liq Density"		,	"kg/m^3"	,
145	,	1147	,	"Junction Liq Velocity"		,	"m/s"	,
151	,	1033	,	"Junction V/I Energy"		,	"J/kg"	,
149	,	1122	,	"Junction Vap Density"		,	"kg/m^3"	,
146	,	1147	,	"Junction Vap Velocity"		,	"m/s"	,
213	,	1000	,	"KNUDSEN NUMBER"		,	" "	,
186	,	178	,	"LEIDENFROST Temperature"	,	"F"	,
187	,	179	,	"LEIDENFROST Temperature"	,	"K"	,
211	,	1000	,	"LEWIS NUMBER"			,	" "	,
67	,	183	,	"Length"			,	"cm"	,
345	,	182	,	"Length"			,	"ft"	,
8	,	181	,	"Length"			,	"in"	,
268	,	1206	,	"Level"				,	"mv"	,
442	,	1000	,	"Limit Switch Position"		,	" "	,
128	,	188	,	"Liquid Density"		,	"kg/m^3"	,
226	,	187	,	"Liquid Density"		,	"lbm/ft^3"	,
91	,	192	,	"Liquid Level"			,	"cm"	,
55	,	190	,	"Liquid Level"			,	"in"	,
249	,	193	,	"Liquid Level"			,	"m"	,
99	,	1076	,	"Liquid Phase Velocity"		,	"ft/s"	,
169	,	1034	,	"Liquid Specific Heat"		,	"J/kg*K"	,
168	,	194	,	"Liquid Specific Heat"		,	"btu/lbm*F"	,
161	,	204	,	"Liquid Viscosity"		,	"cp"	,
160	,	203	,	"Liquid Viscosity"		,	"lbm/ft*hr"	,
133	,	1000	,	"Liquid Void Fraction"		,	" "	,
325	,	1111	,	"Local Heat Generation"		,	"kW/ft"	,
276	,	1112	,	"Local Heat Generation"		,	"kW/m"	,
219	,	207	,	"MACH NUMBER"			,	" "	,
217	,	208	,	"MARTINELLI NUMBER"		,	" "	,
229	,	210	,	"Mass"				,	"kg"	,
246	,	210	,	"Mass"				,	"kg"	,
413	,	210	,	"Mass"				,	"kg"	,
46	,	209	,	"Mass"				,	"lbm"	,
245	,	1216	,	"Mass"				,	"mg"	,
41	,	1133	,	"Mass Balance"			,	"lbm"	,
397	,	215	,	"Mass Flow"			,	"kg/s"	,
408	,	215	,	"Mass Flow"			,	"kg/s"	,
79	,	215	,	"Mass Flow"			,	"kg/s"	,
298	,	212	,	"Mass Flow"			,	"lbm/hr"	,
365	,	212	,	"Mass Flow"			,	"lbm/hr"	,
25	,	211	,	"Mass Flow"			,	"lbm/s"	,
102	,	1139	,	"Mass Flow / Vol"		,	"lbm/ft^3*s"	,
157	,	215	,	"Mass Flow Rate"		,	"kg/s"	,
328	,	211	,	"Mass Flow Rate"		,	"lbm/s"	,
383	,	214	,	"Mass Flow Rate"		,	"mlbm/hr"	,
424	,	1123	,	"Mass Flow Rate into Tank"	,	"kg/s"	,
33	,	220	,	"Mass Flux"			,	"10e6 lbm/hr*ft^2"	,
411	,	221	,	"Mass Flux"			,	"kg/s*m^2"	,
78	,	221	,	"Mass Flux"			,	"kg/s*m^2"	,
306	,	219	,	"Mass Flux"			,	"lb/hr*ft^2"	,
24	,	218	,	"Mass Flux"			,	"lbm/s*ft^2"	,
343	,	218	,	"Mass Velocity"			,	"lbm/hr*ft^2"	,
10	,	1026	,	"Material Temperature"		,	"F"	,
68	,	1035	,	"Material Temperature"		,	"K"	,
156	,	1035	,	"Mesh Point Temperature"	,	"K"	,
340	,	1026	,	"Metal Temperature"		,	"F"	,
275	,	1035	,	"Metal Temperature"		,	"K"	,
357	,	235	,	"Moments"			,	"N*m"	,
356	,	233	,	"Moments"			,	"lbf*in"	,
126	,	237	,	"Momentum Flux"			,	"10e3 lbm/ft*s^2"	,
81	,	238	,	"Momentum Flux"			,	"kg/m*s^2"	,
27	,	236	,	"Momentum Flux"			,	"lbm/ft*s^2"	,
233	,	239	,	"Momentum Flux"			,	"mg/m*s^2"	,
215	,	244	,	"NUSSELT NUMBER"		,	" "	,
375	,	1188	,	"Neutron Detector"		,	"W/cm"	,
107	,	1000	,	"Neutron Detectors"		,	" "	,
336	,	1208	,	"Neutron Detectors"		,	"nano amps"	,
372	,	241	,	"Neutron Flux"			,	"10X13 n/cm^2*s"	,
116	,	240	,	"Neutron Flux"			,	"n/cm^2*s"	,
32	,	242	,	"Normalized Power"		,	" "	,
48	,	1047	,	"Normalized Pump Torque"	,	"N*m"	,
119	,	1035	,	"Outlet Temperature"		,	"K"	,
396	,	1035	,	"Outlet Temperature"		,	"K"	,
410	,	1191	,	"Outlet Temperature"		,	"channel"	,
414	,	1214	,	"Outlet Temperature"		,	"uV"	,
392	,	245	,	"Oxidation-Reduction-Pot"	,	"mV"	,
216	,	258	,	"PRANDTL NUMBER"		,	" "	,
124	,	258	,	"Peak Flux"			,	"n/cm^2*s"	,
401	,	251	,	"Percent"			,	"%"	,
56	,	251	,	"Percent"			,	" "	,
44	,	252	,	"Period"			,	"s"	,
97	,	1035	,	"Photo Tube Temperature"	,	"K"	,
438	,	1120	,	"Pitot Tube DP"			,	"kg/m*s^2"	,
434	,	1160	,	"Pitot Tube Location"		,	"mm"	,
439	,	1160	,	"Pitot Tube Location"		,	"mm"	,
432	,	1052	,	"Pitot Tube Output"		,	"V"	,
313	,	1052	,	"Potential"			,	"V"	,
282	,	1004	,	"Power"				,	"%"	,
371	,	257	,	"Power"				,	"GW"	,
42	,	256	,	"Power"				,	"MW"	,
181	,	253	,	"Power"				,	"btu/hr"	,
18	,	255	,	"Power"				,	"kW"	,
373	,	1111	,	"Power"				,	"kW/ft"	,
228	,	1112	,	"Power"				,	"kW/m"	,
334	,	1112	,	"Power"				,	"kW/m"	,
295	,	1209	,	"Power"				,	"np"	,
152	,	1054	,	"Power Input"			,	"W"	,
406	,	264	,	"Pressure"			,	"KPa"	,
407	,	264	,	"Pressure"			,	"KPa"	,
398	,	265	,	"Pressure"			,	"MPa"	,
399	,	265	,	"Pressure"			,	"MPa"	,
87	,	265	,	"Pressure"			,	"MPa"	,
347	,	263	,	"Pressure"			,	"Pa"	,
415	,	263	,	"Pressure"			,	"Pa"	,
362	,	262	,	"Pressure"			,	"bar"	,
62	,	264	,	"Pressure"			,	"kPa"	,
386	,	1196	,	"Pressure"			,	"kg/cm^2"	,
14	,	261	,	"Pressure"			,	"psi"	,
15	,	259	,	"Pressure"			,	"psia"	,
3	,	260	,	"Pressure"			,	"psig"	,
239	,	1199	,	"Pump Head"			,	"m^2/s^2"	,
240	,	1148	,	"Pump Momentum Source"		,	"m/s^2"	,
281	,	1107	,	"Pump Power"			,	"kW"	,
430	,	1032	,	"Pump Speed"			,	"Hz"	,
29	,	268	,	"Pump Speed"			,	"rpm"	,
297	,	272	,	"Pump Torque"			,	"%"	,
90	,	1047	,	"Pump Torque"			,	"N*m"	,
296	,	271	,	"Pump Torque"			,	"lbf*ft"	,
31	,	273	,	"Quality"			,	" "	,
221	,	281	,	"RAYLEIGH NUMBER"		,	" "	,
209	,	287	,	"REYNOLDS NUMBER"		,	" "	,
269	,	1138	,	"RHOF"				,	"lbm/ft^3"	,
270	,	1138	,	"RHOG"				,	"lbm/ft^3"	,
271	,	1138	,	"RHOL"				,	"lbm/ft^3"	,
319	,	1147	,	"ROUHANI Liquid Velocity"	,	"m/s"	,
320	,	1147	,	"ROUHANI Vapor Velocity"	,	"m/s"	,
354	,	276	,	"Radians"			,	" "	,
203	,	279	,	"Radius"			,	"cm"	,
200	,	278	,	"Radius"			,	"ft"	,
202	,	277	,	"Radius"			,	"in"	,
201	,	280	,	"Radius"			,	"m"	,
38	,	285	,	"Reactivity"			,	"$"	,
315	,	1088	,	"Reactor Power"			,	"GW"	,
112	,	1042	,	"Reactor Power"			,	"MW"	,
351	,	1054	,	"Reactor Power"			,	"W"	,
123	,	1040	,	"Rod Internal Pressure"		,	"MPa"	,
378	,	1173	,	"Rod Internal Pressure"		,	"psia"	,
285	,	1145	,	"Rod Position"			,	"m"	,
360	,	1147	,	"Rotation Speed"		,	"m/s"	,
352	,	1211	,	"Rotations"			,	"rad"	,
115	,	1207	,	"S-P Neutron Detector Curr"	,	"na"	,
214	,	1000	,	"STABILITY NUMBER"		,	" "	,
222	,	315	,	"STANTON NUMBER"		,	" "	,
225	,	1000	,	"STROUHAL NUMBER"		,	" "	,
263	,	1213	,	"SYSTEM SECONDS"		,	"ss"	,
288	,	299	,	"Saturation Pressure"		,	"MPa"	,
286	,	297	,	"Saturation Pressure"		,	"Pa"	,
287	,	298	,	"Saturation Pressure"		,	"kPa"	,
47	,	296	,	"Saturation Pressure"		,	"psia"	,
248	,	302	,	"Saturation Temperature"	,	"C"	,
22	,	300	,	"Saturation Temperature"	,	"F"	,
75	,	301	,	"Saturation Temperature"	,	"K"	,
103	,	304	,	"Slip Ratio"			,	" "	,
236	,	306	,	"Specific Entropy"		,	"btu/lbm*R"	,
237	,	305	,	"Specific Entropy"		,	"kJ/kg*K"	,
167	,	309	,	"Specific Heat"			,	"J/kg*K"	,
166	,	308	,	"Specific Heat"			,	"btu/lbm*F"	,
130	,	1033	,	"Specific Int Energy"		,	"J/kg"	,
131	,	1033	,	"Specific Liq Int Energy"	,	"J/kg"	,
132	,	1033	,	"Specific Vap Int Energy"	,	"J/kg"	,
12	,	312	,	"Specific Volume"		,	"ft^3/lbm"	,
69	,	313	,	"Specific Volume"		,	"m^3/kg"	,
106	,	1000	,	"Steam Quality"			,	" "	,
39	,	317	,	"Stored Energy"			,	"Btu"	,
4	,	319	,	"Strain"			,	" "	,
310	,	1203	,	"Strain"			,	"microm/m"	,
63	,	321	,	"Strain"			,	"mm/m"	,
21	,	333	,	"Surface Temperature"		,	"F"	,
74	,	334	,	"Surface Temperature"		,	"K"	,
165	,	337	,	"Surface Tension"		,	"N/m"	,
164	,	336	,	"Surface Tension"		,	"lbf/ft"	,
258	,	336	,	"TOTAL CP TIME"			,	"s"	,
259	,	336	,	"TOTAL I/O TIME"		,	"s"	,
188	,	1026	,	"T[wall] - T[sat]"		,	"F"	,
189	,	1035	,	"T[wall] - T[sat]"		,	"K"	,
374	,	1128	,	"Tank Level"			,	"l"	,
242	,	340	,	"Temperature"			,	"C"	,
34	,	338	,	"Temperature"			,	"F"	,
84	,	339	,	"Temperature"			,	"K"	,
327	,	1025	,	"Temperature Difference"	,	"C"	,
326	,	1035	,	"Temperature Difference"	,	"K"	,
96	,	1035	,	"Temperature Difference"	,	"K"	,
53	,	343	,	"Thermal Conductivity"		,	"Btu/s*ft*F"	,
92	,	344	,	"Thermal Conductivity"		,	"kW/m*K"	,
174	,	345	,	"Thermal Diffusivity"		,	"ft^2/s"	,
175	,	346	,	"Thermal Diffusivity"		,	"m^2/s"	,
105	,	347	,	"Thermodynamic Quality"		,	" "	,
77 	,	1000	,	"Time"				,	" "	,
176	,	1000	,	"Time"				,	" "	,
364	,	356	,	"Time"				,	"min"	,
36	,	355	,	"Time"				,	"s"	,
86	,	355	,	"Time"				,	"s"	,
317	,	1178	,	"Time (s from year 1900)"	,	"s"	,
52	,	359	,	"Time After Reflood"		,	"s"	,
88	,	359	,	"Time After Reflood"		,	"s"	,
177	,	1000	,	"Time After Rupture"		,	" "	,
35	,	1178	,	"Time After Rupture"		,	"s"	,
85	,	1178	,	"Time After Rupture"		,	"s"	,
178	,	1000	,	"Time To CHF"			,	" "	,
127	,	1122	,	"Total Density"			,	"kg/m^3"	,
37	,	1007	,	"Total Energy"			,	"Btu"	,
309	,	1186	,	"Total Energy"			,	"J"	,
43	,	1020	,	"Total Heat Removed"		,	"Btu/s"	,
58	,	1081	,	"Total Volume"			,	"ft^3"	,
443	,	1000	,	"Unknown"			,	" "	,
445	,	1000	,	"Unknown"			,	" "	,
446	,	1000	,	"Unknown"			,	" "	,
447	,	1000	,	"Unknown"			,	" "	,
448	,	1000	,	"Unknown"			,	" "	,
449	,	1000	,	"Unknown"			,	" "	,
450	,	1000	,	"Unknown"			,	" "	,
433	,	1004	,	"Valve Pos. Control Signal"	,	"%"	,
346	,	1052	,	"Valve Position"		,	"V"	,
108	,	1000	,	"Valve Position"		,	" "	,
109	,	1000	,	"Valve Position"		,	" "	,
267	,	1206	,	"Valve Position"		,	"mv"	,
129	,	370	,	"Vapor Density"			,	"kg/m^3"	,
227	,	369	,	"Vapor Density"			,	"lbm/ft^3"	,
100	,	1076	,	"Vapor Phase Velocity"		,	"ft/s"	,
171	,	1034	,	"Vapor Specific Heat"		,	"J/kg*K"	,
170	,	372	,	"Vapor Specific Heat"		,	"btu/lbm*F"	,
182	,	379	,	"Vapor Velocity"		,	"ft/s"	,
183	,	380	,	"Vapor Velocity"		,	"m/s"	,
163	,	381	,	"Vapor Viscosity"		,	"cp"	,
162	,	382	,	"Vapor Viscosity"		,	"lbm/ft*hr"	,
134	,	1000	,	"Vapor Void Fraction"		,	" "	,
235	,	386	,	"Velocity"			,	"m/s"	,
431	,	1202	,	"Vibration Amplitude (rms)"	,	"micro m"	,
159	,	389	,	"Viscosity"			,	"cp"	,
158	,	388	,	"Viscosity"			,	"lbm/ft*hr"	,
95	,	390	,	"Void Fraction"			,	" "	,
420	,	1004	,	"Void Fraction - Cond.Probe"	,	"%"	,
384	,	1189	,	"Vol Nuc Heat Power"		,	"W/m^3"	,
234	,	393	,	"Voltage"			,	"V"	,
9	,	1000	,	"Voltage"			,	" "	,
416	,	1196	,	"Voltage"			,	"kg/cm^2"	,
300	,	392	,	"Voltage"			,	"mV"	,
204	,	396	,	"Volume"			,	"ft^3"	,
304	,	395	,	"Volume"			,	"in^3"	,
205	,	400	,	"Volume"			,	"m^3"	,
303	,	398	,	"Volume"			,	"mm^3"	,
143	,	1035	,	"Volume Equil Temperature"	,	"K"	,
139	,	1000	,	"Volume Equilibrium Quality"	,	" "	,
140	,	1054	,	"Volume Heat Source"		,	"W"	,
141	,	1035	,	"Volume Liquid Temperature"	,	"K"	,
135	,	1147	,	"Volume Liquid Velocity"	,	"m/s"	,
137	,	1051	,	"Volume Pressure"		,	"Pa"	,
144	,	439	,	"Volume Sonic Velocity"		,	"m/s"	,
138	,	1000	,	"Volume Static Quality"		,	" "	,
142	,	1035	,	"Volume Vapor Temperature"	,	"K"	,
136	,	1147	,	"Volume Vapor Velocity"		,	"m/s"	,
49	,	1083	,	"Volumetric Flow"		,	"ft^3/s"	,
5	,	1087	,	"Volumetric Flow"		,	"gpm"	,
64	,	1129	,	"Volumetric Flow"		,	"l/s"	,
349	,	1200	,	"Volumetric Flow"		,	"m^3/hr"	,
94	,	1204	,	"Volumetric Flow"		,	"ml/s"	,
444	,	1193	,	"Volumetric Flow (ACFM)"	,	"ft^3/min"	,
395	,	402	,	"Volumetric Flow Rate"		,	"gpm"	,
333	,	403	,	"Volumetric Flow Rate"		,	"l/s"	,
241	,	404	,	"Volumetric Flow Rate"		,	"m^3/s"	,
441	,	1212	,	"Volumetric Flow Rate"		,	"scfm"	,
323	,	1147	,	"Volumetric Liquid Velocity"	,	"m/s"	,
324	,	1147	,	"Volumetric Vapor Velocity"	,	"m/s"	,
210	,	411	,	"WEBER NUMBER"			,	" "	,
437	,	1052	,	"Wall Temperature Output"	,	"V"
};
struct Converts printmatch(int inunit){
	int i;
	for (i=0; i < (int)NKEYS; i++){
		if(eunits[i].eunit == inunit){
                   if(VERBOSE){
         		fprintf(stderr,"converting units and lables %d ",eunits[i].eunit);
         		fprintf(stderr,"%d ",eunits[i].ushunit);
         		fprintf(stderr,"%s ",eunits[i].label_str);
         		fprintf(stderr,"%s ",eunits[i].unit_str);
         		fprintf(stderr,"\n");
	           }
		   return(eunits[i]);
	   }
	}
	return(eunits[0]);
	}

/*<---------------
int main( int argc, char *argv[] ){
	struct Converts printmatch(int inunit);
	struct Converts match;
	match=printmatch(100);
		fprintf(stdout,"%d ",match.eunit);
		fprintf(stdout,"%d ",match.ushunit);
		fprintf(stdout,"%s ",match.label_str);
		fprintf(stdout,"%s ",match.unit_str);
		fprintf(stdout,"\n");
}
---------------->*/

struct pibHeader {
   char fileTyp[80];    /* string indicating file type */
   int size;            /* not used enter 0 */
   int numOfChnls;      /* the number of data channels contained in this file */
   int numOfFiles;         /* the number of data files in the file list */
   char fromfile[80][256]; /* a list of up to 80 filenames */
   int fromfiletype[80];   /* corresponding file types */
   char tofile[256];       /* the name of the file being created */
};

bool_t xdr_fileHead(XDR *xdrs,  struct pibHeader *hdr,int *head_count) {
   int i;
   bool_t rc;
   u_int slen;
   char *cptr;
   cptr = hdr->fileTyp;
   slen = 80;
   /*
    #NRCDB 1.1 ftype=\
    #NRCDB V2.0, K. R. Jones, user:databank, Mon Nov  1 15:04:39 1999              \
    #NRCDB 1.2 header size=0\
    #NRCDB 1.3 number of data channels=1785\
    #NRCDB 1.4 number of filenames=1\
    #NRCDB 1.5 file string 0=/databank/TWX/719.twx                                                         \
    #NRCDB 1.6 file type 0=1000\
    /databank/testdata/ISDMS/719/719.bin
    */

	    /* *
	     *
	     * Strings may be truncated
	     *
	     * */

   if(VERBOSE)fprintf(stderr,"starting to read file header\n");
   if(!(rc = xdr_string(xdrs,  &cptr, slen))) return rc;
   fprintf(stdout," #NRCDB 1.1 ftype=\\\n #%-.78s\\\n",cptr);
   if(!(rc = xdr_int(xdrs, &hdr->size))) return rc;
   fprintf(stdout," #NRCDB 1.2 header size=%d\\\n",hdr->size);
   if(!(rc = xdr_int(xdrs, &hdr->numOfChnls))) return rc;
   fprintf(stdout," #NRCDB 1.3 number of data channels=%d\\\n",hdr->numOfChnls);
   if(!(rc = xdr_int(xdrs, &hdr->numOfFiles))) return rc;
   fprintf(stdout," #NRCDB 1.4 number of filenames=%d\\\n",hdr->numOfFiles);

   for(i=0; i<hdr->numOfFiles; i++) {
       cptr = &hdr->fromfile[i][0];
       slen = 256;
       if(!(rc = xdr_string(xdrs,  &cptr, slen))) return rc;
       fprintf(stdout," #NRCDB 1.5 file string %d=%-.50s\\\n",i,cptr);
   }
   for(i=0; i<hdr->numOfFiles; i++) {
       if(!(rc = xdr_int(xdrs, &hdr->fromfiletype[i]))) return rc;
       fprintf(stdout," #NRCDB 1.6 file type %d=%d\\\n",i,hdr->fromfiletype[i]);
   }
   cptr = &hdr->tofile[0];
   slen = 256;
   if(!(rc = xdr_string(xdrs,  &cptr, slen))) return rc;
   fprintf(stdout," %-.79s\n",cptr);
   *head_count=hdr->numOfChnls;
   return rc;
}
/*
The following code segment demonstrates how to create the XDR stream
and read the header using the xdr_fileHead routine.  It assumes that
the filename and numChan variables (corresponding to the name of the
file to be created, and the number of data channels, respectively)
have been previously defined.
*/
void readFileHeader(XDR *xdrs, char filename[],int *head_count){
   /* declarations */
   FILE *fileHndl;
   struct pibHeader hdr;
   int rc;
   if(VERBOSE)fprintf(stderr,"Reading File Header ...\n");
   /* open the input file */
   fileHndl=fopen(filename,"r");
   if (fileHndl == NULL) {
      fprintf(stderr, "Unable to open file %s", filename);
      return;
   };

   /* create the XDR object */
   xdrstdio_create(xdrs, fileHndl, XDR_DECODE);
   /* read the header structure from the XDR stream */
   rc = xdr_fileHead(xdrs, &hdr,head_count);
   if(!rc) {
      fprintf(stderr,"Error reading PIB file...");
      return;
   }
   if(VERBOSE)fprintf(stderr,"Done Reading File Header\n");
}
/*

4. Compression
The PIB data is compressed on an individual channel basis, as opposed to
full file compression.  This approach provides an efficient method of data
retrieval, permitting direct access to the channel data and eliminating
the need to uncompress the entire file prior to extracting the data
for an individual channel.  The compression method used is defined by
the compression mode or cmpMode entry of the channel header record.
Currently, three compression modes are available as indicated in Table 4.

 Table 4. Channel Compression Modes

  Compression      Description
      Mode

       0           No compression used.
       1           Flat data channel. Single value written to file.
       2           Double precision run length encoded compression.

Typically, compression is turned off for a channel (cmpMode = 0) if
the achievable compression falls below 5%.  This arbitrary threshold
balances the potential savings in storage requirements against the
overhead associated with uncompressing the data.

If the data channel does not vary over the entire range of time, a
compression mode of cmpMode=1 is used.  A single value is then stored
in the channel data block to represent the entire range of data.

The double precision run length encoding compression method is typically
used if the achievable compression exceeds 5%.  Although it is a very
simple algorithm, significant compression is achievable with very
little impact on performance.  In this method, each set of identical,
consecutive values is replaced by two values; the first value being
the number of values replaced and the second being the actual value.
Regions that are not compressed are preceded by a negative value that
indicates the length of the uncompressed region.

Table 5 illustrates application of the algorithm to a set of raw values.
The initial two values 518.3 and 518.4 are replaced by the set of values
 2.0, 518.4 and 518.4.  The  2.0 value indicates that the next two values
are not compressed.  This is followed by the value 518.5 repeated twelve
times which is replaced by the pair 12.0, 518.5.  Similarly, the next
four values have no repeats and are preceded by a value of -4.0, while
the last eight values repeat and are replaced by the pair 8.0,
518.9.

 Table 5. Example of Run Length Compression

   Raw Data          Compressed
                        Data
    518.3               -2.0
    518.4              518.3
    518.5              518.4
    518.5               12.0
    518.5              518.5
    518.5               -4.0
    518.5              518.6
    518.5              518.9
    518.5              518.6
    518.5              518.8
    518.5                8.0
    518.5              518.9
    518.5
    518.5
    518.6
    518.9
    518.6
    518.8
    518.9
    518.9
    518.9
    518.9
    518.9
    518.9
    518.9
    518.9

The following routine can be used to uncompress the data

   Function: uncmpres
    Purpose: uncompress an array of double precision numbers
  Arguments:
      data on entry contains compressed channel data
           on exit contains raw channel data
    dblbuf pointer to temporary array space
     csize on entry contains length of compressed channel data
     fsize on entry contains length of raw channel data
        cm on entry contains compression mode
              0 = no compression
              1 = flat channel
              2 = run length compression
    Returns:
       0
 */
int uncmpres(double *data, double *dblbuf, u_int* csize, int fsize, int cm) {
   int  i,j,k;
   int difs, reps;
   switch (cm) {
   case 0:
             break;     /* no compression */
   case 1:
      for(i = 0; i<fsize; i++) {
        data[i] = data[0];
      }
      break;     /* flat */
   case 2:
      k = i = 0;
      while(i<*csize) {
         if(data[i] < 0.0) {
            difs = (int)( -1*data[i] + 0.1 );
            i++;
            for(j = 0; j<difs; j++) {
               dblbuf[k++] = data[i++];
            }
         } else if(data[i] > 0.0) {
            reps = (int)( data[i] + 0.1 );
            for(j = 0; j<reps; j++) {
               dblbuf[k++] = data[i+1];
            }
            i+=2;
         } else {
            return 1; /* Error uncompressing data cm=2 */
         }
      }
      if(k != fsize) {
         return 2;  /* Error uncompressing data cm=2 */
      }
      for(i = 0; i<fsize; i++) {
         data[i] = dblbuf[i];
      }
   break;  /* run length compression */
   }
   return 0;
}
/*
B.       Channel Header Block

The channel header block immediately follows the file header block. The
channel header block contains one channel header record for each data
channel.  The layout of the channel header record is illustrated in
Table 2 along with a description of each field.  The channel name is
stored in ASCII characters and must be padded with NULL characters to
a length of 24 bytes.  This ensures a fixed size for the channel header
record, allowing the channel header block to be read very efficiently.
Each channel is assigned a unique zero based index that is used to
identify the channel.

              Table 2. Channel Header Record Layout
   XDR            Field         Description
 Primitive       Contents
  xdr_bytes   name[24]   The channel name. Must be NULL padded to 24 bytes.
  xdr_int     Index      A zero based index used to identify the channel.
  xdr_int     size       Number of data points for the channel.
  xdr_int     totalSize  The length of the data in bytes. (8 * size)
  xdr_int     timeIndex  The Index value for the channel containing the time
                         values.
                         Note: This value should be zero for time channels.
  xdr_int     ptrToData  A pointer to the dependent data.
  xdr_int     ptrToTime  A pointer to the independent (time channel) data.
  xdr_int     eucode     An integer value indicating the engineering unit code
                         for this channel. See Section 2, Engineering Unit
                         Codes.
  xdr_int     recNo      Reserved, enter 0.
  xdr_int     orgIndex   The original sequence number used in the source file.
                         (optional)
  xdr_int     orgFile    A zero based index indicating the source file from the
                         file list of the file header block. (optional)
  xdr_int     status     Reserved, enter 0.
  xdr_int     cmpMode    Compression mode. See Section 3, Compression.
  xdr_int     cmpSize    Size of compressed data array.
  xdr_int     spare1     Reserved, enter 0.
  xdr_int     spare2     Reserved, enter 0.
  xdr_int     spare3     Reserved, enter 0.


The most difficult fields to populate are the pointers to the data and
time data, ptrToData and ptrToTime, respectively.  These pointers are
most easily determined using the xdr_getpos function prior to writing the
channel data.  Unfortunately, these values are not known at the time the
channel header block is written.  For this reason, the channel header
block is written in two passes.  During the first pass, write the file
header block, the channel header block and then the data block.  As the
data block is written, use the xdr_getpos function to determine the file
pointer prior to writing each data channel.  After the data block has
been written, rewind the file to the start of the channel header block
and rewrite the channel header block.

The following xdr_chanHead routine can be used to read or write a single
channel header record to an existing XDR stream. The channel data is
retrieved from or placed into a pibChnlRec structure as defined below:

   Source Listing 3. Channel Header XDR Access Routine
*/
struct pibChnlRec {
   char name[24];
   int  Index;
   int  size;
   int  totalSize;
   int  timeIndex;
   int  ptrToData;
   int  ptrToTime;
   int  eucode;
   int  recNo;
   int  orgIndex;
   int  orgFile;
   int  status;
   int  cmpMode;
   int  cmpSize;
   int  spare1;
   int  spare2;
   int  spare3;
};
bool_t xdr_chanHead(XDR *xdrs, struct pibChnlRec *Chan) {
   bool_t rc;
   u_int slen = 24;
   char *cptr;
   struct Converts printmatch(int inunit);
   struct Converts match;

   cptr = Chan->name;

   if(VERBOSE)fprintf(stderr,"read or write channel header\n");
   rc = (xdr_bytes(xdrs, &cptr, &slen, slen)
      && xdr_int(xdrs, &Chan->Index)
      && xdr_int(xdrs, &Chan->size)
      && xdr_int(xdrs, &Chan->totalSize)
      && xdr_int(xdrs, &Chan->timeIndex)
      && xdr_int(xdrs, &Chan->ptrToData)
      && xdr_int(xdrs, &Chan->ptrToTime)
      && xdr_int(xdrs, &Chan->eucode)
      && xdr_int(xdrs, &Chan->recNo)
      && xdr_int(xdrs, &Chan->orgIndex)
      && xdr_int(xdrs, &Chan->orgFile)
      && xdr_int(xdrs, &Chan->status)
      && xdr_int(xdrs, &Chan->cmpMode)
      && xdr_int(xdrs, &Chan->cmpSize)
      && xdr_int(xdrs, &Chan->spare1)
      && xdr_int(xdrs, &Chan->spare2)
      && xdr_int(xdrs, &Chan->spare3));


/*
Units do not map into ours unless we add to our file and add a number
varid*8, alpha*20 do not map to Chan->name*24

*/
   match=printmatch(Chan->eucode); /* convert eunits to ush(1) units if perfect match, else set string for label */

   fprintf(stdout," code.... %5.5d varid... %8s\n",Chan->Index,cptr);
   fprintf(stdout," node.... %5.5d",Chan->recNo);
   fprintf(stdout," second.. %5.5d",Chan->orgIndex);
   fprintf(stdout," tertiary %5.5d\n",Chan->orgFile);
   fprintf(stdout," unit.... %5.5d\n",match.ushunit);
   fprintf(stdout," alpha... %s\n",cptr);
   fprintf(stdout," altlegnd NRCDB %-15s index=%-5d eucode=%-5d\n", cptr,Chan->Index,Chan->eucode);
   if(Chan->timeIndex == 0 ){
      fprintf(stdout," xref.... %5.5d\n",Chan->timeIndex); /* WARNING: Assume this is a time curve */
   }else{
      fprintf(stdout," xref.... %5.5d\n",Chan->timeIndex+1);
   }

   fprintf(stdout," values.. %d\n",Chan->size);
   fprintf(stdout," skips... 00000\n");
   fprintf(stdout," data_rec 00000\n");
   fprintf(stdout," head_rec 00000\n");
   fprintf(stdout," zref.... 00000\n");
   fprintf(stdout," minvalue   0.000000000000     maxvalue   0.000000000000\n");
   fprintf(stdout," unit_lab %s\n",match.label_str);
   fprintf(stdout," rpad....   0.000000000000       0.000000000000       0.000000000000\n");
   fprintf(stdout,"            0.000000000000       0.000000000000       0.000000000000\n");
   fprintf(stdout,"            0.000000000000       0.000000000000       0.000000000000\n");
   if(VERBOSE){
      fprintf(stderr,"   name =%s\n",cptr);
      fprintf(stderr,"   Index =%d\n",Chan->Index);
      fprintf(stderr,"   size =%d\n",Chan->size);
      fprintf(stderr,"   totalSize =%d\n",Chan->totalSize);
      fprintf(stderr,"   timeIndex =%d\n",Chan->timeIndex);
      fprintf(stderr,"   ptrToData =%d\n",Chan->ptrToData);
      fprintf(stderr,"   ptrToTime =%d\n",Chan->ptrToTime);
      fprintf(stderr,"   eucode =%d\n",Chan->eucode);
      fprintf(stderr,"   recNo =%d\n",Chan->recNo);
      fprintf(stderr,"   orgIndex =%d\n",Chan->orgIndex);
      fprintf(stderr,"   orgFile =%d\n",Chan->orgFile);
      fprintf(stderr,"   status =%d\n",Chan->status);
      fprintf(stderr,"   cmpMode =%d\n",Chan->cmpMode);
      fprintf(stderr,"   cmpSize =%d\n",Chan->cmpSize);
      fprintf(stderr,"   spare1 =%d\n",Chan->spare1);
      fprintf(stderr,"   spare2 =%d\n",Chan->spare2);
      fprintf(stderr,"   spare3 =%d\n",Chan->spare3);
      fprintf(stderr,"   reading channel header\n");
   }
   return rc;
}

/*

Typically, an array of pibChnlRec structures will be allocated and
filled with the channel information, then the xdr_chanHead routine will
be called once for each element of the array.

C.          Channel Data Block

The channel data block follows immediately after the channel header
block.  It consists entirely of arrays of double precision floating
point data output with the xdr routines.  The channel data is written
using the xdr_array primitive with a double precision element type.
The following code segment illustrates reading the channel data block
from an open XDR stream positioned to the data:

Channel Data Block Read
*/
#ifdef CYGWIN
#define DATABUFFSIZE 100000 // SHOULD CHECK NOT EXCEEDED
#else
#define DATABUFFSIZE 300000 // SHOULD CHECK NOT EXCEEDED
#endif
void xdr_readChannel(XDR *xdrs ,struct pibChnlRec *Chan) {
   /* declarations */
   bool_t rc;
	 u_int csize; /* length of data array compressed */
	 u_int csizeraw; /* length of data array uncompressed */
         double data[DATABUFFSIZE]; /* pointer to data */
         double dbuf[DATABUFFSIZE]; /* pointer to data */
         double *dptr; /* pointer to data */
         double *dbptr; /* pointer to data buffer*/
         int i;
         int j;
	 int cmpMode;

	 if(VERBOSE)fprintf(stderr,"\n");
	 csize=Chan->cmpSize;
	 csizeraw=Chan->size;
	 cmpMode=Chan->cmpMode;

	 dptr=&data[0];
	 dbptr=&dbuf[0];

	 if(VERBOSE)fprintf(stderr,"get data for channel size=%d\n",csize);
         /* read channel data */
         rc = xdr_array(xdrs,          /* XDR stream */
            (char **)&dptr, /* pointer to the data to be written */
            (u_int *)&csize,/* number of elements */
            csize,          /* maximum number of elements */
            sizeof(double), /* element size */
            (xdrproc_t)xdr_double);    /* primitive used to encode element */
         if(!rc) {
            fprintf(stderr,"Error reading input file data. exiting");
            exit(-1);
         }
         /* uncompress the data */
	 if(VERBOSE)fprintf(stderr,"uncompressing data size=%d compression mode=%d\n",csize,cmpMode);
         rc=uncmpres(dptr,dbptr, &csize,csizeraw,cmpMode);
	 if(VERBOSE)fprintf(stderr,"uncompressed data size=%d\n",csizeraw);

	 fprintf(stdout," ");
         for(j=0; j<5 ; j++){
	     fprintf(stdout,"x....................");
	 }

	 for(i=0;i<csizeraw;i++){
	   if(i%5 == 0)fprintf(stdout,"\n |");
           fprintf(stdout,"%20lf|",data[i]);
	 }
	 fprintf(stdout,"\n");
}
/*
After the data block has been written, and the channel header record
structures have been updated to include the compression and file pointer
information, the file is rewound to the start of the channel header block,
and the channel header block is overwritten.
*/


int main(int argc, char *argv[]) {
   XDR xdrs;
   int header_count;
   int i;
   u_int filePos;
   u_int diditPos;
   struct pibChnlRec chanHead;

   if(argc < 3 ){
	   VERBOSE=0;
   }else{
           VERBOSE=1;
   }

   readFileHeader(&xdrs,argv[1],&header_count);
   for(i=0;i <header_count;i++){
      fprintf(stdout," ------------------------------------------------------------------- Curve %d\n",i+1);
      if(VERBOSE) fprintf(stderr,"header=%d\n",i);
      xdr_chanHead(&xdrs, &chanHead) ;

      filePos=xdr_getpos(&xdrs);                     /* get position where reading header left you    */
      diditPos=xdr_setpos(&xdrs,chanHead.ptrToData); /* where curve data is                           */
      xdr_readChannel(&xdrs, &chanHead);             /* read curve data                               */
      diditPos=xdr_setpos(&xdrs,filePos);            /* return to where left off reading header data  */
   }
   fprintf(stdout," This header ends the data file ...\n");
   fprintf(stdout," code.... 00000 varid... END     \n");
   fprintf(stdout," node.... 00000 second.. 00000 tertiary 00000\n");
   fprintf(stdout," unit.... 00000\n");
   fprintf(stdout," alpha...                     \n");
   fprintf(stdout," altlegnd                                                             \n");
   fprintf(stdout," xref.... 00000\n");
   fprintf(stdout," values.. 00000\n");
   fprintf(stdout," skips... 00000\n");
   fprintf(stdout," data_rec 00000\n");
   fprintf(stdout," head_rec 00000\n");
   fprintf(stdout," zref.... 00000\n");
   fprintf(stdout," minvalue   0.000000000000     maxvalue   0.000000000000\n");
   fprintf(stdout," unit_lab  \n");
   fprintf(stdout," rpad....   0.000000000000       0.000000000000       0.000000000000    \n");
   fprintf(stdout,"            0.000000000000       0.000000000000       0.000000000000    \n");
   fprintf(stdout,"            0.000000000000       0.000000000000       0.000000000000    \n");
   fprintf(stdout," x....................x....................x....................x....................x....................\n");

}
