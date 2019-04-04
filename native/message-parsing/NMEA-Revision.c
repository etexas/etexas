/*
 * Generated by asn1c-0.9.26 (http://lionet.info/asn1c)
 * From ASN.1 module "DSRC"
 * 	found in "DSRC_R36_Source.ASN"
 */

#include "NMEA-Revision.h"

int
NMEA_Revision_constraint(asn_TYPE_descriptor_t *td, const void *sptr,
			asn_app_constraint_failed_f *ctfailcb, void *app_key) {
	/* Replace with underlying type checker */
	td->check_constraints = asn_DEF_NativeEnumerated.check_constraints;
	return td->check_constraints(td, sptr, ctfailcb, app_key);
}

/*
 * This type is implemented using NativeEnumerated,
 * so here we adjust the DEF accordingly.
 */
static void
NMEA_Revision_1_inherit_TYPE_descriptor(asn_TYPE_descriptor_t *td) {
	td->free_struct    = asn_DEF_NativeEnumerated.free_struct;
	td->print_struct   = asn_DEF_NativeEnumerated.print_struct;
	td->check_constraints = asn_DEF_NativeEnumerated.check_constraints;
	td->ber_decoder    = asn_DEF_NativeEnumerated.ber_decoder;
	td->der_encoder    = asn_DEF_NativeEnumerated.der_encoder;
	td->xer_decoder    = asn_DEF_NativeEnumerated.xer_decoder;
	td->xer_encoder    = asn_DEF_NativeEnumerated.xer_encoder;
	td->uper_decoder   = asn_DEF_NativeEnumerated.uper_decoder;
	td->uper_encoder   = asn_DEF_NativeEnumerated.uper_encoder;
	if(!td->per_constraints)
		td->per_constraints = asn_DEF_NativeEnumerated.per_constraints;
	td->elements       = asn_DEF_NativeEnumerated.elements;
	td->elements_count = asn_DEF_NativeEnumerated.elements_count;
     /* td->specifics      = asn_DEF_NativeEnumerated.specifics;	// Defined explicitly */
}

void
NMEA_Revision_free(asn_TYPE_descriptor_t *td,
		void *struct_ptr, int contents_only) {
	NMEA_Revision_1_inherit_TYPE_descriptor(td);
	td->free_struct(td, struct_ptr, contents_only);
}

int
NMEA_Revision_print(asn_TYPE_descriptor_t *td, const void *struct_ptr,
		int ilevel, asn_app_consume_bytes_f *cb, void *app_key) {
	NMEA_Revision_1_inherit_TYPE_descriptor(td);
	return td->print_struct(td, struct_ptr, ilevel, cb, app_key);
}

asn_dec_rval_t
NMEA_Revision_decode_ber(asn_codec_ctx_t *opt_codec_ctx, asn_TYPE_descriptor_t *td,
		void **structure, const void *bufptr, size_t size, int tag_mode) {
	NMEA_Revision_1_inherit_TYPE_descriptor(td);
	return td->ber_decoder(opt_codec_ctx, td, structure, bufptr, size, tag_mode);
}

asn_enc_rval_t
NMEA_Revision_encode_der(asn_TYPE_descriptor_t *td,
		void *structure, int tag_mode, ber_tlv_tag_t tag,
		asn_app_consume_bytes_f *cb, void *app_key) {
	NMEA_Revision_1_inherit_TYPE_descriptor(td);
	return td->der_encoder(td, structure, tag_mode, tag, cb, app_key);
}

asn_dec_rval_t
NMEA_Revision_decode_xer(asn_codec_ctx_t *opt_codec_ctx, asn_TYPE_descriptor_t *td,
		void **structure, const char *opt_mname, const void *bufptr, size_t size) {
	NMEA_Revision_1_inherit_TYPE_descriptor(td);
	return td->xer_decoder(opt_codec_ctx, td, structure, opt_mname, bufptr, size);
}

asn_enc_rval_t
NMEA_Revision_encode_xer(asn_TYPE_descriptor_t *td, void *structure,
		int ilevel, enum xer_encoder_flags_e flags,
		asn_app_consume_bytes_f *cb, void *app_key) {
	NMEA_Revision_1_inherit_TYPE_descriptor(td);
	return td->xer_encoder(td, structure, ilevel, flags, cb, app_key);
}

static asn_INTEGER_enum_map_t asn_MAP_NMEA_Revision_value2enum_1[] = {
	{ 0,	7,	"unknown" },
	{ 1,	8,	"reserved" },
	{ 10,	4,	"rev1" },
	{ 20,	4,	"rev2" },
	{ 30,	4,	"rev3" },
	{ 40,	4,	"rev4" },
	{ 50,	4,	"rev5" }
	/* This list is extensible */
};
static unsigned int asn_MAP_NMEA_Revision_enum2value_1[] = {
	1,	/* reserved(1) */
	2,	/* rev1(10) */
	3,	/* rev2(20) */
	4,	/* rev3(30) */
	5,	/* rev4(40) */
	6,	/* rev5(50) */
	0	/* unknown(0) */
	/* This list is extensible */
};
static asn_INTEGER_specifics_t asn_SPC_NMEA_Revision_specs_1 = {
	asn_MAP_NMEA_Revision_value2enum_1,	/* "tag" => N; sorted by tag */
	asn_MAP_NMEA_Revision_enum2value_1,	/* N => "tag"; sorted by N */
	7,	/* Number of elements in the maps */
	8,	/* Extensions before this member */
	1,	/* Strict enumeration */
	0,	/* Native long size */
	0
};
static ber_tlv_tag_t asn_DEF_NMEA_Revision_tags_1[] = {
	(ASN_TAG_CLASS_UNIVERSAL | (10 << 2))
};
asn_TYPE_descriptor_t asn_DEF_NMEA_Revision = {
	"NMEA-Revision",
	"NMEA-Revision",
	NMEA_Revision_free,
	NMEA_Revision_print,
	NMEA_Revision_constraint,
	NMEA_Revision_decode_ber,
	NMEA_Revision_encode_der,
	NMEA_Revision_decode_xer,
	NMEA_Revision_encode_xer,
	0, 0,	/* No PER support, use "-gen-PER" to enable */
	0,	/* Use generic outmost tag fetcher */
	asn_DEF_NMEA_Revision_tags_1,
	sizeof(asn_DEF_NMEA_Revision_tags_1)
		/sizeof(asn_DEF_NMEA_Revision_tags_1[0]), /* 1 */
	asn_DEF_NMEA_Revision_tags_1,	/* Same as above */
	sizeof(asn_DEF_NMEA_Revision_tags_1)
		/sizeof(asn_DEF_NMEA_Revision_tags_1[0]), /* 1 */
	0,	/* No PER visible constraints */
	0, 0,	/* Defined elsewhere */
	&asn_SPC_NMEA_Revision_specs_1	/* Additional specs */
};

