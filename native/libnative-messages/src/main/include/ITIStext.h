/*
 * Generated by asn1c-0.9.26 (http://lionet.info/asn1c)
 * From ASN.1 module "ITIS"
 * 	found in "DSRC_R36_Source.ASN"
 */

#ifndef	_ITIStext_H_
#define	_ITIStext_H_


#include <asn_application.h>

/* Including external dependencies */
#include <IA5String.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ITIStext */
typedef IA5String_t	 ITIStext_t;

/* Implementation */
extern asn_TYPE_descriptor_t asn_DEF_ITIStext;
asn_struct_free_f ITIStext_free;
asn_struct_print_f ITIStext_print;
asn_constr_check_f ITIStext_constraint;
ber_type_decoder_f ITIStext_decode_ber;
der_type_encoder_f ITIStext_encode_der;
xer_type_decoder_f ITIStext_decode_xer;
xer_type_encoder_f ITIStext_encode_xer;

#ifdef __cplusplus
}
#endif

#endif	/* _ITIStext_H_ */
#include <asn_internal.h>
