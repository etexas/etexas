/*
 * Generated by asn1c-0.9.26 (http://lionet.info/asn1c)
 * From ASN.1 module "DSRC"
 * 	found in "DSRC_R36_Source.ASN"
 */

#ifndef	_BrakeSystemStatus_H_
#define	_BrakeSystemStatus_H_


#include <asn_application.h>

/* Including external dependencies */
#include <OCTET_STRING.h>

#ifdef __cplusplus
extern "C" {
#endif

/* BrakeSystemStatus */
typedef OCTET_STRING_t	 BrakeSystemStatus_t;

/* Implementation */
extern asn_TYPE_descriptor_t asn_DEF_BrakeSystemStatus;
asn_struct_free_f BrakeSystemStatus_free;
asn_struct_print_f BrakeSystemStatus_print;
asn_constr_check_f BrakeSystemStatus_constraint;
ber_type_decoder_f BrakeSystemStatus_decode_ber;
der_type_encoder_f BrakeSystemStatus_encode_der;
xer_type_decoder_f BrakeSystemStatus_decode_xer;
xer_type_encoder_f BrakeSystemStatus_encode_xer;

#ifdef __cplusplus
}
#endif

#endif	/* _BrakeSystemStatus_H_ */
#include <asn_internal.h>
