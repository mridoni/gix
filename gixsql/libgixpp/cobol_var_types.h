#pragma once

#define COBOL_TYPE_UNSIGNED_NUMBER		1         
#define COBOL_TYPE_SIGNED_NUMBER_TS		2        // (trailing separate)
#define COBOL_TYPE_SIGNED_NUMBER_TC		3        // (trailing combined)
#define COBOL_TYPE_SIGNED_NUMBER_LS		4        // (leading separate)
#define COBOL_TYPE_SIGNED_NUMBER_LC		5        // (leading combined)
#define COBOL_TYPE_UNSIGNED_NUMBER_PD	8		 // packed decimal
#define COBOL_TYPE_SIGNED_NUMBER_PD		9		 // packed decimal
#define COBOL_TYPE_ALPHANUMERIC			16
#define COBOL_TYPE_UNSIGNED_BINARY		22
#define COBOL_TYPE_SIGNED_BINARY		23
#define COBOL_TYPE_JAPANESE				24     
#define COBOL_TYPE_GROUP        		25            
#define COBOL_TYPE_FLOAT				26                                        
#define COBOL_TYPE_DOUBLE				27                                        
#define COBOL_TYPE_NATIONAL				28     

#define COBOL_TYPE_MIN					1 
#define COBOL_TYPE_MAX					28