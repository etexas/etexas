/*
 * Generated by asn1c-0.9.26 (http://lionet.info/asn1c)
 * From ASN.1 module "DSRC"
 * 	found in "DSRC_R36_Source.ASN"
 */

#ifndef	_ProbeVehicleData_H_
#define	_ProbeVehicleData_H_


#include <asn_application.h>

/* Including external dependencies */
#include "DSRCmsgID.h"
#include "ProbeSegmentNumber.h"
#include "FullPositionVector.h"
#include "VehicleType.h"
#include "Count.h"
#include <asn_SEQUENCE_OF.h>
#include <constr_SEQUENCE_OF.h>
#include <constr_SEQUENCE.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Forward declarations */
struct VehicleIdent;
struct Snapshot;

/* ProbeVehicleData */
typedef struct ProbeVehicleData {
	DSRCmsgID_t	 msgID;
	ProbeSegmentNumber_t	*segNum	/* OPTIONAL */;
	struct VehicleIdent	*probeID	/* OPTIONAL */;
	FullPositionVector_t	 startVector;
	VehicleType_t	 vehicleType;
	Count_t	*cntSnapshots	/* OPTIONAL */;
	struct ProbeVehicleData__snapshots {
		A_SEQUENCE_OF(struct Snapshot) list;
		
		/* Context for parsing across buffer boundaries */
		asn_struct_ctx_t _asn_ctx;
	} snapshots;
	/*
	 * This type is extensible,
	 * possible extensions are below.
	 */
	
	/* Context for parsing across buffer boundaries */
	asn_struct_ctx_t _asn_ctx;
} ProbeVehicleData_t;

/* Implementation */
extern asn_TYPE_descriptor_t asn_DEF_ProbeVehicleData;

#ifdef __cplusplus
}
#endif

/* Referred external types */
#include "VehicleIdent.h"
#include "Snapshot.h"

#endif	/* _ProbeVehicleData_H_ */
#include <asn_internal.h>
