/*
 * Generated by asn1c-0.9.26 (http://lionet.info/asn1c)
 * From ASN.1 module "DSRC"
 * 	found in "DSRC_R36_Source.ASN"
 */

#ifndef	_SignalStatusMessage_H_
#define	_SignalStatusMessage_H_


#include <asn_application.h>

/* Including external dependencies */
#include "DSRCmsgID.h"
#include "MsgCount.h"
#include "IntersectionID.h"
#include "IntersectionStatusObject.h"
#include "TransitStatus.h"
#include "SignalState.h"
#include <asn_SEQUENCE_OF.h>
#include <constr_SEQUENCE_OF.h>
#include <constr_SEQUENCE.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Forward declarations */
struct VehicleIdent;

/* SignalStatusMessage */
typedef struct SignalStatusMessage {
	DSRCmsgID_t	 msgID;
	MsgCount_t	 msgCnt;
	IntersectionID_t	 id;
	IntersectionStatusObject_t	 status;
	struct SignalStatusMessage__priority {
		A_SEQUENCE_OF(SignalState_t) list;
		
		/* Context for parsing across buffer boundaries */
		asn_struct_ctx_t _asn_ctx;
	} *priority;
	struct VehicleIdent	*priorityCause	/* OPTIONAL */;
	struct SignalStatusMessage__prempt {
		A_SEQUENCE_OF(SignalState_t) list;
		
		/* Context for parsing across buffer boundaries */
		asn_struct_ctx_t _asn_ctx;
	} *prempt;
	struct VehicleIdent	*preemptCause	/* OPTIONAL */;
	TransitStatus_t	*transitStatus	/* OPTIONAL */;
	/*
	 * This type is extensible,
	 * possible extensions are below.
	 */
	
	/* Context for parsing across buffer boundaries */
	asn_struct_ctx_t _asn_ctx;
} SignalStatusMessage_t;

/* Implementation */
extern asn_TYPE_descriptor_t asn_DEF_SignalStatusMessage;

#ifdef __cplusplus
}
#endif

/* Referred external types */
#include "VehicleIdent.h"

#endif	/* _SignalStatusMessage_H_ */
#include <asn_internal.h>
