/*
 * Generated by asn1c-0.9.26 (http://lionet.info/asn1c)
 * From ASN.1 module "DSRC"
 * 	found in "DSRC_R36_Source.ASN"
 */

#ifndef	_ExitService_H_
#define	_ExitService_H_


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

/* ExitService */
typedef struct ExitService {
	A_SEQUENCE_OF(struct ExitService__Member {
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
} ExitService_t;

/* Implementation */
extern asn_TYPE_descriptor_t asn_DEF_ExitService;

#ifdef __cplusplus
}
#endif

#endif	/* _ExitService_H_ */
#include <asn_internal.h>
