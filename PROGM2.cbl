      **************************************************************************
      * Author: Renan Cicero
      * Date: 12/02/2023
      * Purpose: Desafio Modulo 2
      **************************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGM2.


      **************************************************************************
       DATA DIVISION.
       FILE SECTION.

      **************************************************************************
       WORKING-STORAGE SECTION.
       01  WS-NOTAS.
           03 WS-N1         PIC S99 VALUE 0.
           03 WS-N2         PIC 99 VALUE 0.
           03 WS-N3         PIC 99 VALUE 0.
           03 WS-N4         PIC 99 VALUE 0.
           03 WS-MD         PIC 99 VALUE 0.

       01  WS-TEXT.
           03 WS-NOME       PIC X(30) VALUE SPACE.
           03 WS-MAT        PIC X(12) VALUE SPACE.

       01  WS-STATUS.
           03 WS-AR        PIC X(10) VALUE SPACE.
           03 WS-ST        PIC 9 VALUE 0.


      **************************************************************************
       PROCEDURE DIVISION.

           PERFORM P100-TUDO.
      *>************************************************************************
       P100-TUDO.
           PERFORM P300-RESET
           PERFORM P001-COLETA.
           PERFORM P002-COLETA-N1.
           PERFORM P003-COLETA-N2.
           PERFORM P004-COLETA-N3.
           PERFORM P005-COLETA-N4.
           PERFORM P200-RESULTADO.
           PERFORM P000-FINALIZA.
       P100-FIM.


      *>************************************************************************
       P001-COLETA.
            DISPLAY 'INFORME O NOME DO ALUNO: ' ACCEPT WS-NOME
            DISPLAY 'INFOME A MATERIA: '        ACCEPT WS-MAT
           .
       P001-FIM.


       P002-COLETA-N1.
            DISPLAY 'INFORME A NOTA DO PRIMEIRO TRIMESTRE: '
            ACCEPT WS-N1
            IF WS-N1 NOT> 0 OR > 10
                 DISPLAY 'NOTA INVALIDA - TENTE NOVAMENTE'
                 PERFORM P002-COLETA-N1
            END-IF
            .
       P002-FIM.


       P003-COLETA-N2.
            DISPLAY 'INFORME A NOTA DO SEGUNDO TRIMESTRE: '
            ACCEPT WS-N2
            IF WS-N2 NOT> 0 OR > 10
                 DISPLAY 'NOTA INVALIDA - TENTE NOVAMENTE'
                 PERFORM P003-COLETA-N2
            END-IF
            .
       P003-FIM.


       P004-COLETA-N3.
            DISPLAY 'INFORME A NOTA DO TERCEIRO TRIMESTRE: '
            ACCEPT WS-N3
            IF WS-N3 NOT> 0 OR > 10
                 DISPLAY 'NOTA INVALIDA - TENTE NOVAMENTE'
                 PERFORM P004-COLETA-N3
            END-IF
            .
       P004-FIM.

       P005-COLETA-N4.
            DISPLAY 'INFORME A NOTA DO QUARTO TRIMESTRE: '
            ACCEPT WS-N4
            IF WS-N4 NOT> 0 OR > 10
                 DISPLAY 'NOTA INVALIDA - TENTE NOVAMENTE'
                 PERFORM P005-COLETA-N4
            END-IF
           DISPLAY 'NOTAS INSERIDAS COM SUCESSO'
           .
       P005-FIM.

      *>************************************************************************
       P200-RESULTADO.
           COMPUTE WS-MD = (WS-N1 + WS-N2 + WS-N3 + WS-N4) /  4
           IF WS-MD LESS 7
                MOVE 'REPROVADO' TO WS-AR
           ELSE
                MOVE 'APROVADO' TO WS-AR
           END-IF

           DISPLAY ' '
           DISPLAY '**********RESULTADO DO PROCESSAMENTO***************'
           DISPLAY 'NOME DO ALUNO:..... 'WS-NOME
           DISPLAY 'MATERIA:............'WS-MAT
           DISPLAY 'MEDIA:..............'WS-MD
           DISPLAY 'STATUS:.............'WS-AR
           DISPLAY '***************************************************'
           DISPLAY ' '
           .

           PERFORM P201-REPET.

       P201-REPET.
           DISPLAY 'DESEJA CALCULAR UMA NOVA MEDIA ? (1=Sim/2=Nao)'
           ACCEPT WS-ST

           EVALUATE WS-ST
            WHEN 1
                 PERFORM P100-TUDO
            WHEN 2
                 DISPLAY 'ATE A PROXIMA'
                 PERFORM P000-FINALIZA
            WHEN OTHER
                 DISPLAY 'OPCAO INVALIDA'
                 PERFORM P201-REPET
           END-EVALUATE
           .
       P201-FIM.
           
           
       P200-FIM.

           
       P300-RESET.
           INITIALIZE WS-NOTAS WS-TEXT WS-STATUS.
           DISPLAY '***************************************************'
           .
       P300-FIM.


           
       P000-FINALIZA.
           STOP RUN.
       END PROGRAM PROGM2.
