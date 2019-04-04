/*
 * Generated by asn1c-0.9.26 (http://lionet.info/asn1c)
 * From ASN.1 module "DSRC"
 * 	found in "DSRC_R36_Source.ASN"
 */

#ifndef	_RoadSideAlert_H_
#define	_RoadSideAlert_H_


#include <asn_application.h>

/* Including external dependencies */
#include "DSRCmsgID.h"
#include "MsgCount.h"
#include "ITIScodes.h"
#include "Priority.h"
#include "HeadingSlice.h"
#include "Extent.h"
#include "FurtherInfoID.h"
#include "MsgCRC.h"
#include <asn_SEQUENCE_OF.h>
#include <constr_SEQUENCE_OF.h>
#include <constr_SEQUENCE.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Forward declarations */
struct FullPositionVector;

/* RoadSideAlert */
typedef struct RoadSideAlert {
	DSRCmsgID_t	 msgID;
	MsgCount_t	 msgCnt;
	ITIScodes_t	 typeEvent;
	struct RoadSideAlert__description {
		A_SEQUENCE_OF(ITIScodes_t) list;
		
		/* Context for parsing across buffer boundaries */
		asn_struct_ctx_t _asn_ctx;
	} *description;
	Priority_t	*priority	/* OPTIONAL */;
	HeadingSlice_t	*heading	/* OPTIONAL */;
	Extent_t	*extent	/* OPTIONAL */;
	struct FullPositionVector	*positon	/* OPTIONAL */;
	FurtherInfoID_t	*furtherInfoID	/* OPTIONAL */;
	MsgCRC_t	 crc;
	
	/* Context for parsing across buffer boundaries */
	asn_struct_ctx_t _asn_ctx;
} RoadSideAlert_t;

/* Implementation */
extern asn_TYPE_descriptor_t asn_DEF_RoadSideAlert;

#ifdef __cplusplus
}
#endif

/* Referred external types */
#include "FullPositionVector.h"

#endif	/* _RoadSideAlert_H_ */
#include <asn_internal.h>
