/*
 * Generated by asn1c-0.9.26 (http://lionet.info/asn1c)
 * From ASN.1 module "DSRC"
 * 	found in "DSRC_R36_Source.ASN"
 */

#include "PathHistory.h"

static int
memb_pathHistoryPointSets_01_constraint_5(asn_TYPE_descriptor_t *td, const void *sptr,
			asn_app_constraint_failed_f *ctfailcb, void *app_key) {
	size_t size;
	
	if(!sptr) {
		_ASN_CTFAIL(app_key, td, sptr,
			"%s: value not given (%s:%d)",
			td->name, __FILE__, __LINE__);
		return -1;
	}
	
	/* Determine the number of elements */
	size = _A_CSEQUENCE_FROM_VOID(sptr)->count;
	
	if((size >= 1 && size <= 23)) {
		/* Perform validation of the inner elements */
		return td->check_constraints(td, sptr, ctfailcb, app_key);
	} else {
		_ASN_CTFAIL(app_key, td, sptr,
			"%s: constraint failed (%s:%d)",
			td->name, __FILE__, __LINE__);
		return -1;
	}
}

static int
memb_pathHistoryPointSets_02_constraint_5(asn_TYPE_descriptor_t *td, const void *sptr,
			asn_app_constraint_failed_f *ctfailcb, void *app_key) {
	const OCTET_STRING_t *st = (const OCTET_STRING_t *)sptr;
	size_t size;
	
	if(!sptr) {
		_ASN_CTFAIL(app_key, td, sptr,
			"%s: value not given (%s:%d)",
			td->name, __FILE__, __LINE__);
		return -1;
	}
	
	size = st->size;
	
	if((size >= 15 && size <= 345)) {
		/* Constraint check succeeded */
		return 0;
	} else {
		_ASN_CTFAIL(app_key, td, sptr,
			"%s: constraint failed (%s:%d)",
			td->name, __FILE__, __LINE__);
		return -1;
	}
}

static int
memb_pathHistoryPointSets_03_constraint_5(asn_TYPE_descriptor_t *td, const void *sptr,
			asn_app_constraint_failed_f *ctfailcb, void *app_key) {
	const OCTET_STRING_t *st = (const OCTET_STRING_t *)sptr;
	size_t size;
	
	if(!sptr) {
		_ASN_CTFAIL(app_key, td, sptr,
			"%s: value not given (%s:%d)",
			td->name, __FILE__, __LINE__);
		return -1;
	}
	
	size = st->size;
	
	if((size >= 12 && size <= 276)) {
		/* Constraint check succeeded */
		return 0;
	} else {
		_ASN_CTFAIL(app_key, td, sptr,
			"%s: constraint failed (%s:%d)",
			td->name, __FILE__, __LINE__);
		return -1;
	}
}

static int
memb_pathHistoryPointSets_04_constraint_5(asn_TYPE_descriptor_t *td, const void *sptr,
			asn_app_constraint_failed_f *ctfailcb, void *app_key) {
	const OCTET_STRING_t *st = (const OCTET_STRING_t *)sptr;
	size_t size;
	
	if(!sptr) {
		_ASN_CTFAIL(app_key, td, sptr,
			"%s: value not given (%s:%d)",
			td->name, __FILE__, __LINE__);
		return -1;
	}
	
	size = st->size;
	
	if((size >= 8 && size <= 184)) {
		/* Constraint check succeeded */
		return 0;
	} else {
		_ASN_CTFAIL(app_key, td, sptr,
			"%s: constraint failed (%s:%d)",
			td->name, __FILE__, __LINE__);
		return -1;
	}
}

static int
memb_pathHistoryPointSets_05_constraint_5(asn_TYPE_descriptor_t *td, const void *sptr,
			asn_app_constraint_failed_f *ctfailcb, void *app_key) {
	const OCTET_STRING_t *st = (const OCTET_STRING_t *)sptr;
	size_t size;
	
	if(!sptr) {
		_ASN_CTFAIL(app_key, td, sptr,
			"%s: value not given (%s:%d)",
			td->name, __FILE__, __LINE__);
		return -1;
	}
	
	size = st->size;
	
	if((size >= 10 && size <= 230)) {
		/* Constraint check succeeded */
		return 0;
	} else {
		_ASN_CTFAIL(app_key, td, sptr,
			"%s: constraint failed (%s:%d)",
			td->name, __FILE__, __LINE__);
		return -1;
	}
}

static int
memb_pathHistoryPointSets_06_constraint_5(asn_TYPE_descriptor_t *td, const void *sptr,
			asn_app_constraint_failed_f *ctfailcb, void *app_key) {
	const OCTET_STRING_t *st = (const OCTET_STRING_t *)sptr;
	size_t size;
	
	if(!sptr) {
		_ASN_CTFAIL(app_key, td, sptr,
			"%s: value not given (%s:%d)",
			td->name, __FILE__, __LINE__);
		return -1;
	}
	
	size = st->size;
	
	if((size >= 6 && size <= 138)) {
		/* Constraint check succeeded */
		return 0;
	} else {
		_ASN_CTFAIL(app_key, td, sptr,
			"%s: constraint failed (%s:%d)",
			td->name, __FILE__, __LINE__);
		return -1;
	}
}

static int
memb_pathHistoryPointSets_07_constraint_5(asn_TYPE_descriptor_t *td, const void *sptr,
			asn_app_constraint_failed_f *ctfailcb, void *app_key) {
	const OCTET_STRING_t *st = (const OCTET_STRING_t *)sptr;
	size_t size;
	
	if(!sptr) {
		_ASN_CTFAIL(app_key, td, sptr,
			"%s: value not given (%s:%d)",
			td->name, __FILE__, __LINE__);
		return -1;
	}
	
	size = st->size;
	
	if((size >= 11 && size <= 242)) {
		/* Constraint check succeeded */
		return 0;
	} else {
		_ASN_CTFAIL(app_key, td, sptr,
			"%s: constraint failed (%s:%d)",
			td->name, __FILE__, __LINE__);
		return -1;
	}
}

static int
memb_pathHistoryPointSets_08_constraint_5(asn_TYPE_descriptor_t *td, const void *sptr,
			asn_app_constraint_failed_f *ctfailcb, void *app_key) {
	const OCTET_STRING_t *st = (const OCTET_STRING_t *)sptr;
	size_t size;
	
	if(!sptr) {
		_ASN_CTFAIL(app_key, td, sptr,
			"%s: value not given (%s:%d)",
			td->name, __FILE__, __LINE__);
		return -1;
	}
	
	size = st->size;
	
	if((size >= 7 && size <= 161)) {
		/* Constraint check succeeded */
		return 0;
	} else {
		_ASN_CTFAIL(app_key, td, sptr,
			"%s: constraint failed (%s:%d)",
			td->name, __FILE__, __LINE__);
		return -1;
	}
}

static int
memb_pathHistoryPointSets_09_constraint_5(asn_TYPE_descriptor_t *td, const void *sptr,
			asn_app_constraint_failed_f *ctfailcb, void *app_key) {
	const OCTET_STRING_t *st = (const OCTET_STRING_t *)sptr;
	size_t size;
	
	if(!sptr) {
		_ASN_CTFAIL(app_key, td, sptr,
			"%s: value not given (%s:%d)",
			td->name, __FILE__, __LINE__);
		return -1;
	}
	
	size = st->size;
	
	if((size >= 9 && size <= 196)) {
		/* Constraint check succeeded */
		return 0;
	} else {
		_ASN_CTFAIL(app_key, td, sptr,
			"%s: constraint failed (%s:%d)",
			td->name, __FILE__, __LINE__);
		return -1;
	}
}

static int
memb_pathHistoryPointSets_10_constraint_5(asn_TYPE_descriptor_t *td, const void *sptr,
			asn_app_constraint_failed_f *ctfailcb, void *app_key) {
	const OCTET_STRING_t *st = (const OCTET_STRING_t *)sptr;
	size_t size;
	
	if(!sptr) {
		_ASN_CTFAIL(app_key, td, sptr,
			"%s: value not given (%s:%d)",
			td->name, __FILE__, __LINE__);
		return -1;
	}
	
	size = st->size;
	
	if((size >= 5 && size <= 104)) {
		/* Constraint check succeeded */
		return 0;
	} else {
		_ASN_CTFAIL(app_key, td, sptr,
			"%s: constraint failed (%s:%d)",
			td->name, __FILE__, __LINE__);
		return -1;
	}
}

static asn_TYPE_member_t asn_MBR_pathHistoryPointSets_01_6[] = {
	{ ATF_POINTER, 0, 0,
		(ASN_TAG_CLASS_UNIVERSAL | (16 << 2)),
		0,
		&asn_DEF_PathHistoryPointType_01,
		0,	/* Defer constraints checking to the member type */
		0,	/* PER is not compiled, use -gen-PER */
		0,
		""
		},
};
static ber_tlv_tag_t asn_DEF_pathHistoryPointSets_01_tags_6[] = {
	(ASN_TAG_CLASS_CONTEXT | (0 << 2)),
	(ASN_TAG_CLASS_UNIVERSAL | (16 << 2))
};
static asn_SET_OF_specifics_t asn_SPC_pathHistoryPointSets_01_specs_6 = {
	sizeof(struct PathHistory__crumbData__pathHistoryPointSets_01),
	offsetof(struct PathHistory__crumbData__pathHistoryPointSets_01, _asn_ctx),
	0,	/* XER encoding is XMLDelimitedItemList */
};
static /* Use -fall-defs-global to expose */
asn_TYPE_descriptor_t asn_DEF_pathHistoryPointSets_01_6 = {
	"pathHistoryPointSets-01",
	"pathHistoryPointSets-01",
	SEQUENCE_OF_free,
	SEQUENCE_OF_print,
	SEQUENCE_OF_constraint,
	SEQUENCE_OF_decode_ber,
	SEQUENCE_OF_encode_der,
	SEQUENCE_OF_decode_xer,
	SEQUENCE_OF_encode_xer,
	0, 0,	/* No PER support, use "-gen-PER" to enable */
	0,	/* Use generic outmost tag fetcher */
	asn_DEF_pathHistoryPointSets_01_tags_6,
	sizeof(asn_DEF_pathHistoryPointSets_01_tags_6)
		/sizeof(asn_DEF_pathHistoryPointSets_01_tags_6[0]) - 1, /* 1 */
	asn_DEF_pathHistoryPointSets_01_tags_6,	/* Same as above */
	sizeof(asn_DEF_pathHistoryPointSets_01_tags_6)
		/sizeof(asn_DEF_pathHistoryPointSets_01_tags_6[0]), /* 2 */
	0,	/* No PER visible constraints */
	asn_MBR_pathHistoryPointSets_01_6,
	1,	/* Single element */
	&asn_SPC_pathHistoryPointSets_01_specs_6	/* Additional specs */
};

static asn_TYPE_member_t asn_MBR_crumbData_5[] = {
	{ ATF_NOFLAGS, 0, offsetof(struct PathHistory__crumbData, choice.pathHistoryPointSets_01),
		(ASN_TAG_CLASS_CONTEXT | (0 << 2)),
		0,
		&asn_DEF_pathHistoryPointSets_01_6,
		memb_pathHistoryPointSets_01_constraint_5,
		0,	/* PER is not compiled, use -gen-PER */
		0,
		"pathHistoryPointSets-01"
		},
	{ ATF_NOFLAGS, 0, offsetof(struct PathHistory__crumbData, choice.pathHistoryPointSets_02),
		(ASN_TAG_CLASS_CONTEXT | (1 << 2)),
		-1,	/* IMPLICIT tag at current level */
		&asn_DEF_OCTET_STRING,
		memb_pathHistoryPointSets_02_constraint_5,
		0,	/* PER is not compiled, use -gen-PER */
		0,
		"pathHistoryPointSets-02"
		},
	{ ATF_NOFLAGS, 0, offsetof(struct PathHistory__crumbData, choice.pathHistoryPointSets_03),
		(ASN_TAG_CLASS_CONTEXT | (2 << 2)),
		-1,	/* IMPLICIT tag at current level */
		&asn_DEF_OCTET_STRING,
		memb_pathHistoryPointSets_03_constraint_5,
		0,	/* PER is not compiled, use -gen-PER */
		0,
		"pathHistoryPointSets-03"
		},
	{ ATF_NOFLAGS, 0, offsetof(struct PathHistory__crumbData, choice.pathHistoryPointSets_04),
		(ASN_TAG_CLASS_CONTEXT | (3 << 2)),
		-1,	/* IMPLICIT tag at current level */
		&asn_DEF_OCTET_STRING,
		memb_pathHistoryPointSets_04_constraint_5,
		0,	/* PER is not compiled, use -gen-PER */
		0,
		"pathHistoryPointSets-04"
		},
	{ ATF_NOFLAGS, 0, offsetof(struct PathHistory__crumbData, choice.pathHistoryPointSets_05),
		(ASN_TAG_CLASS_CONTEXT | (4 << 2)),
		-1,	/* IMPLICIT tag at current level */
		&asn_DEF_OCTET_STRING,
		memb_pathHistoryPointSets_05_constraint_5,
		0,	/* PER is not compiled, use -gen-PER */
		0,
		"pathHistoryPointSets-05"
		},
	{ ATF_NOFLAGS, 0, offsetof(struct PathHistory__crumbData, choice.pathHistoryPointSets_06),
		(ASN_TAG_CLASS_CONTEXT | (5 << 2)),
		-1,	/* IMPLICIT tag at current level */
		&asn_DEF_OCTET_STRING,
		memb_pathHistoryPointSets_06_constraint_5,
		0,	/* PER is not compiled, use -gen-PER */
		0,
		"pathHistoryPointSets-06"
		},
	{ ATF_NOFLAGS, 0, offsetof(struct PathHistory__crumbData, choice.pathHistoryPointSets_07),
		(ASN_TAG_CLASS_CONTEXT | (6 << 2)),
		-1,	/* IMPLICIT tag at current level */
		&asn_DEF_OCTET_STRING,
		memb_pathHistoryPointSets_07_constraint_5,
		0,	/* PER is not compiled, use -gen-PER */
		0,
		"pathHistoryPointSets-07"
		},
	{ ATF_NOFLAGS, 0, offsetof(struct PathHistory__crumbData, choice.pathHistoryPointSets_08),
		(ASN_TAG_CLASS_CONTEXT | (7 << 2)),
		-1,	/* IMPLICIT tag at current level */
		&asn_DEF_OCTET_STRING,
		memb_pathHistoryPointSets_08_constraint_5,
		0,	/* PER is not compiled, use -gen-PER */
		0,
		"pathHistoryPointSets-08"
		},
	{ ATF_NOFLAGS, 0, offsetof(struct PathHistory__crumbData, choice.pathHistoryPointSets_09),
		(ASN_TAG_CLASS_CONTEXT | (8 << 2)),
		-1,	/* IMPLICIT tag at current level */
		&asn_DEF_OCTET_STRING,
		memb_pathHistoryPointSets_09_constraint_5,
		0,	/* PER is not compiled, use -gen-PER */
		0,
		"pathHistoryPointSets-09"
		},
	{ ATF_NOFLAGS, 0, offsetof(struct PathHistory__crumbData, choice.pathHistoryPointSets_10),
		(ASN_TAG_CLASS_CONTEXT | (9 << 2)),
		-1,	/* IMPLICIT tag at current level */
		&asn_DEF_OCTET_STRING,
		memb_pathHistoryPointSets_10_constraint_5,
		0,	/* PER is not compiled, use -gen-PER */
		0,
		"pathHistoryPointSets-10"
		},
};
static asn_TYPE_tag2member_t asn_MAP_crumbData_tag2el_5[] = {
    { (ASN_TAG_CLASS_CONTEXT | (0 << 2)), 0, 0, 0 }, /* pathHistoryPointSets-01 */
    { (ASN_TAG_CLASS_CONTEXT | (1 << 2)), 1, 0, 0 }, /* pathHistoryPointSets-02 */
    { (ASN_TAG_CLASS_CONTEXT | (2 << 2)), 2, 0, 0 }, /* pathHistoryPointSets-03 */
    { (ASN_TAG_CLASS_CONTEXT | (3 << 2)), 3, 0, 0 }, /* pathHistoryPointSets-04 */
    { (ASN_TAG_CLASS_CONTEXT | (4 << 2)), 4, 0, 0 }, /* pathHistoryPointSets-05 */
    { (ASN_TAG_CLASS_CONTEXT | (5 << 2)), 5, 0, 0 }, /* pathHistoryPointSets-06 */
    { (ASN_TAG_CLASS_CONTEXT | (6 << 2)), 6, 0, 0 }, /* pathHistoryPointSets-07 */
    { (ASN_TAG_CLASS_CONTEXT | (7 << 2)), 7, 0, 0 }, /* pathHistoryPointSets-08 */
    { (ASN_TAG_CLASS_CONTEXT | (8 << 2)), 8, 0, 0 }, /* pathHistoryPointSets-09 */
    { (ASN_TAG_CLASS_CONTEXT | (9 << 2)), 9, 0, 0 } /* pathHistoryPointSets-10 */
};
static asn_CHOICE_specifics_t asn_SPC_crumbData_specs_5 = {
	sizeof(struct PathHistory__crumbData),
	offsetof(struct PathHistory__crumbData, _asn_ctx),
	offsetof(struct PathHistory__crumbData, present),
	sizeof(((struct PathHistory__crumbData *)0)->present),
	asn_MAP_crumbData_tag2el_5,
	10,	/* Count of tags in the map */
	0,
	-1	/* Extensions start */
};
static /* Use -fall-defs-global to expose */
asn_TYPE_descriptor_t asn_DEF_crumbData_5 = {
	"crumbData",
	"crumbData",
	CHOICE_free,
	CHOICE_print,
	CHOICE_constraint,
	CHOICE_decode_ber,
	CHOICE_encode_der,
	CHOICE_decode_xer,
	CHOICE_encode_xer,
	0, 0,	/* No PER support, use "-gen-PER" to enable */
	CHOICE_outmost_tag,
	0,	/* No effective tags (pointer) */
	0,	/* No effective tags (count) */
	0,	/* No tags (pointer) */
	0,	/* No tags (count) */
	0,	/* No PER visible constraints */
	asn_MBR_crumbData_5,
	10,	/* Elements count */
	&asn_SPC_crumbData_specs_5	/* Additional specs */
};

static asn_TYPE_member_t asn_MBR_PathHistory_1[] = {
	{ ATF_POINTER, 3, offsetof(struct PathHistory, initialPosition),
		(ASN_TAG_CLASS_CONTEXT | (0 << 2)),
		-1,	/* IMPLICIT tag at current level */
		&asn_DEF_FullPositionVector,
		0,	/* Defer constraints checking to the member type */
		0,	/* PER is not compiled, use -gen-PER */
		0,
		"initialPosition"
		},
	{ ATF_POINTER, 2, offsetof(struct PathHistory, currGPSstatus),
		(ASN_TAG_CLASS_CONTEXT | (1 << 2)),
		-1,	/* IMPLICIT tag at current level */
		&asn_DEF_GPSstatus,
		0,	/* Defer constraints checking to the member type */
		0,	/* PER is not compiled, use -gen-PER */
		0,
		"currGPSstatus"
		},
	{ ATF_POINTER, 1, offsetof(struct PathHistory, itemCnt),
		(ASN_TAG_CLASS_CONTEXT | (2 << 2)),
		-1,	/* IMPLICIT tag at current level */
		&asn_DEF_Count,
		0,	/* Defer constraints checking to the member type */
		0,	/* PER is not compiled, use -gen-PER */
		0,
		"itemCnt"
		},
	{ ATF_NOFLAGS, 0, offsetof(struct PathHistory, crumbData),
		(ASN_TAG_CLASS_CONTEXT | (3 << 2)),
		+1,	/* EXPLICIT tag at current level */
		&asn_DEF_crumbData_5,
		0,	/* Defer constraints checking to the member type */
		0,	/* PER is not compiled, use -gen-PER */
		0,
		"crumbData"
		},
};
static ber_tlv_tag_t asn_DEF_PathHistory_tags_1[] = {
	(ASN_TAG_CLASS_UNIVERSAL | (16 << 2))
};
static asn_TYPE_tag2member_t asn_MAP_PathHistory_tag2el_1[] = {
    { (ASN_TAG_CLASS_CONTEXT | (0 << 2)), 0, 0, 0 }, /* initialPosition */
    { (ASN_TAG_CLASS_CONTEXT | (1 << 2)), 1, 0, 0 }, /* currGPSstatus */
    { (ASN_TAG_CLASS_CONTEXT | (2 << 2)), 2, 0, 0 }, /* itemCnt */
    { (ASN_TAG_CLASS_CONTEXT | (3 << 2)), 3, 0, 0 } /* crumbData */
};
static asn_SEQUENCE_specifics_t asn_SPC_PathHistory_specs_1 = {
	sizeof(struct PathHistory),
	offsetof(struct PathHistory, _asn_ctx),
	asn_MAP_PathHistory_tag2el_1,
	4,	/* Count of tags in the map */
	0, 0, 0,	/* Optional elements (not needed) */
	3,	/* Start extensions */
	5	/* Stop extensions */
};
asn_TYPE_descriptor_t asn_DEF_PathHistory = {
	"PathHistory",
	"PathHistory",
	SEQUENCE_free,
	SEQUENCE_print,
	SEQUENCE_constraint,
	SEQUENCE_decode_ber,
	SEQUENCE_encode_der,
	SEQUENCE_decode_xer,
	SEQUENCE_encode_xer,
	0, 0,	/* No PER support, use "-gen-PER" to enable */
	0,	/* Use generic outmost tag fetcher */
	asn_DEF_PathHistory_tags_1,
	sizeof(asn_DEF_PathHistory_tags_1)
		/sizeof(asn_DEF_PathHistory_tags_1[0]), /* 1 */
	asn_DEF_PathHistory_tags_1,	/* Same as above */
	sizeof(asn_DEF_PathHistory_tags_1)
		/sizeof(asn_DEF_PathHistory_tags_1[0]), /* 1 */
	0,	/* No PER visible constraints */
	asn_MBR_PathHistory_1,
	4,	/* Elements count */
	&asn_SPC_PathHistory_specs_1	/* Additional specs */
};

