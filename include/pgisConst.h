/* -----------------------------------------------------------------------------
 *
 * LWTYPE numbers, used internally by PostGIS
 *
 * ---------------------------------------------------------------------------*/

#define POINTTYPE                1
#define LINETYPE                 2
#define POLYGONTYPE              3
#define MULTIPOINTTYPE           4
#define MULTILINETYPE            5
#define MULTIPOLYGONTYPE         6
#define COLLECTIONTYPE           7
#define CIRCSTRINGTYPE           8
#define COMPOUNDTYPE             9
#define CURVEPOLYTYPE           10
#define MULTICURVETYPE          11
#define MULTISURFACETYPE        12
#define POLYHEDRALSURFACETYPE   13
#define TRIANGLETYPE            14
#define TINTYPE                 15
#define NUMTYPES                16

/* -----------------------------------------------------------------------------
 *
 * Flags applied in EWKB to indicate Z/M dimensions and
 * presence/absence of SRID and bounding boxes
 *
 * ---------------------------------------------------------------------------*/

#define WKBZOFFSET  0x80000000
#define WKBMOFFSET  0x40000000
#define WKBSRIDFLAG 0x20000000
#define WKBBBOXFLAG 0x10000000

