/*
 * Generated by asn1c-0.9.26 (http://lionet.info/asn1c)
 * From ASN.1 module "DSRC"
 * 	found in "DSRC_R36_Source.ASN"
 */

#ifndef	_GenericSignage_H_
#define	_GenericSignage_H_


#include <asn_application.h>

/* Including external dependencies */
#include <asn_SEQUENCE_OF.h>
#include "ITIScodes.h"
#include <IA5String.h>
#include <constr_CHOICE.h>
#include <constr_SEQUENCE.h>
#include <constr_SEQUENCE_OF.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Dependencies */
typedef enum item_PR {
	item_PR_NOTHING,	/* No components present */
	item_PR_itis,
	item_PR_text
} item_PR;

/* GenericSignage */
typedef struct GenericSignage {
	A_SEQUENCE_OF(struct GenericSignage__Member {
		struct item {
			item_PR present;
			union item_u {
				ITIScodes_t	 itis;
				IA5String_t	 text;
			} choice;
			
			/* Context for parsing across buffer boundaries */
			asn_struct_ctx_t _asn_ctx;
		} item;
		
		/* Context for parsing across buffer boundaries */
		asn_struct_ctx_t _asn_ctx;
	} ) list;
	
	/* Context for parsing across buffer boundaries */
	asn_struct_ctx_t _asn_ctx;
} GenericSignage_t;

/* Implementation */
extern asn_TYPE_descriptor_t asn_DEF_GenericSignage;

#ifdef __cplusplus
}
#endif

#endif	/* _GenericSignage_H_ */
#include <asn_internal.h>