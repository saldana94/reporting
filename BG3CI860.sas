
DATA SALIDA1;
SET X.ITCC;
*matching cuentas;
if SubStr(CUENTA,1,4) NE '0011' THEN
  DO; REG_SALIDA=REG_ENTRADA;
	END;
ELSE IF E1_CUENTA_NROPER = E2_CUENTA_NROPER THEN
DO; ENCONTRADOS =  +1;
	REG_SALIDA = REG_ENTRADA;
	S1_CODIGO_PAIS = SubStr(E2_CODIGOSWI,5,2);
	END;
	* DISPLAY 'ENCONTRO OP.EXTRANJERO EN MOV.ITC: '
 E1-CUENTA ' - ' E2-NUMEROMOV '- PAIS:' S1-CODIGO-PAIS
;

	*201301105-INI
IF E1_CODI_OPE = '01' AND E2_TIPCUENTA = '01' ;
*IF   E1_CODI_OPE = '01' AND E2_TIPCUENTA = '1' ;
	*201301105-FIN;

IF E2_TIPOTRANSF = 'R' THEN
DO; S1_CODI_OPE = '12';
	END;
ELSE
DO; S1_CODI_OPE = '14';
	END;
	*PERFORM GRABA-SALIDA
PERFORM LEE-MOVIMIENTOS
PERFORM LEE-EXTRANJERO
;

ELSE
IF E1_CUENTA_NROPER < E2_CUENTA_NROPER THEN
DO; REG_SALIDA = REG_ENTRADA;
	END;
	*PERFORM GRABA-SALIDA;
	*PERFORM GRABA-MOVIMIENTOS;
ELSE IF E2_TIPCUENTA = '01' THEN
DO; * DISPLAY ' '
                         DISPLAY '*OJO* NO SE ENCONTRO OP.EXT:'
                                 E2-OPERACION ' - ' WS-E2-CUENTA-18 '-'
                                 WS-E2-NUMER-OPE-9 ' EN MOV.ITC'
      *                  DISPLAY 'E1-CUENTA-NROPER:' WS-E1-CUENTA-NROPER
      *                  DISPLAY 'E2-CUENTA-NROPER:' WS-E2-CUENTA-NROPER
                      END-IF
                      PERFORM LEE-EXTRANJERO
                   END-IF
                END-IF
*            END-IF
           END-PERFORM.;

*LEE MOVIMIENTOS;
	* READ E1DQSEQ1 AT END MOVE 1 TO SW-FIN1.;
IF SW_FIN1 = '0' THEN
  DO; LEIDOS1 = +1;
	END;
ELSE
  DO; E1-CUENTA = HIGH-VALUES;
	END;

*LEE EXTRANJERO;
	* READ E2DQSEQ2 AT END MOVE 1 TO SW-FIN2.;
IF SW-FIN2 = '0' THEN
  DO; LEIDOS2 = +1;
	END;
ELSE
  DO; E2-CUENTATRA = HIGH-VALUES;
	END;

*GRABA-SALIDA;
IF S1-PAIS = 'PER' THEN
  DO; CONTRIB_NACIONAL = +1;
	END;
ELSE
  DO; CONTRIB_EXTRANJ = +1;
	END-IF.
	*WRITE REG-SALIDA.;
 DO; GRABADOS = +1;
