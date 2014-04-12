%%-------------------------------------------------------------------------%%
% Programa: grec.cmd
% Data    : 12/06/95
% Autor   : Aldrin Monteiro
% Funcao  : Emissao de Recibos    Ult.Alt : 25/07/96
% Alteracao: incluir tp_recibo 4 quando for restante
%            14/10/1997 - ler a loja da numeradora pela vg_loja_recibo A.M
%            18 03 1998 - Novas tabelas de juros (GP1 e GP2)
% Alterado: Retirada As Mensagens Dos Campos Para Agilizar o Sistema do Radio
%           Fco Filho - 18.12.1998
% 14 11 2000  -  Proteger o campo nr_tab para usar sempre a tabela 2
% 05 09 2002  -  Tp_Fin -> Tp_Fat   Fco Filho
% 13.09.2004  -  Qdo Tp_Fin=3 e Tp_Fin_Rel=2 Gravar Com Tp_Fin=2 - Fco Filho
% 17.09.2004  -  Executado apartir do Caixa (Fl_Caixa = "I") - Silva Jr
% 06.11.2004  -  F7 P/ Permitir Desconto Classe=DE(gsenha2)  - Fco Filho
%                Nr_Minuta_Aut -> Recebe Matricula da Senha
%                No_Historico -> Recebe Codigo da Senha
% 01.06.2005  -  Desconto Aut. de 5% nos pagamentos integral de prestacao
%                durante o Aniversario Paraiba (01/06/05 - 31/08/05) - Silva Jr
% 04 09 2008  -  desconto somente sem valor juros - A.M
% 30 09 2008  -  Executa prog RREDUCAO para pagamento antecipado - murilo
% 01 10 2008  -  Usar senha classe=wq  para pagamento antecipado - murilo
% 09 10 2008  -  Ajustar para trabalhar com os dois sistema de senha
% 15 10 2008  -  Desconto antecipado para prestacoes a vencer
% 07 12 2010  -  Seq porconta no Cd_Almoxarifado - A.M
% 17 01 2011 Ajustado para Oracle - Fco Filho
%%-------------------------------------------------------------------------%%
%% vg_Uf  - (variavel p/ tp_recibo )
%  n1 - (des_juros anterior)

                       %--Busca Informacoes de Contratos AON
                       % Tipo = 1 -> Busca Contrato Claudino Depois AON
                       %        2 -> Busca Contrato AON Depois Claudino
LocalProcedure l_AON(l_Tipo) Local(l_ChaAON)
On DeadLock
   QUITTRANSACTION
   Mens('Conflito, Tente Novamente',10,10)
   Let Vg_VlConta[98] = 9999 Vg_VlConta[99] = 9999
   Return
EndOn

Let Vg_VlConta[98] = 0 \ %-- Vl Recibo AON
    Vg_VlConta[99] = 0 \   %-- Vl Juros AON
    l_ChaAON = ' '

If l_Tipo = '1' % Busca Claudino
   Compute 1 Conjugado Wh Cd_Chave_OUT = \
    $Concat(frecibo2.Sg_Loja_Rec,' 2',frecibo2.Tp_Fin,frecibo2.Nr_Contrato) \
      Eval(Let l_ChaAON = Conjugado.Cd_Chave_Con)
Else
   Compute 1 Conjugado Wh Cd_Chave_Con = \
    $Concat(frecibo2.Sg_Loja_Rec,' 2',frecibo2.Tp_Fin,frecibo2.Nr_Contrato) \
      Eval(Let l_ChaAON = Conjugado.Cd_Chave_Out)
EndIf
If $MemberCount = 0
   Return
EndIf

Compute 1 Contrato Wh Cd_Chave_Con = l_ChaAON And \
   Dt_Cancel = 0 And Dt_Exclui = 0 And Vl_Saldo > 0 Eval('')
If $MemberCount = 0
   Return
ElseIf l_Tipo = '2' % Existe Claudino Aberto
   Mens('Emitir Recibo do Contrato Claudino',10,10)
   Let frecibo2.Sg_Loja_Rec = ' '
   Return
EndIf

Find Prestacao Wh Cd_Chave_Con = l_ChaAON \
   Sorted By Cd_Chave -> sPrestAON

Find 1 sPrestAON Wh Vl_Saldo_Crs > 0 -> sPrestAON
Let Vg_VlConta[98] = Prestacao.Vl_Saldo_Crs vvalor = Vg_VlConta[98]
CALCJURO2(frecibo2.dt_vencimento,Data1,vvalor)
Let Vg_VlConta[99] = vValor
EndProcedure

localprocedure Checa_tp_venda(l_vg_cartao) local(l_tpvenda)
let l_tpvenda = " "
TRANSACTION READ
Compute 1 cartao_prop_loc wh cd_cliente = $ToChar(l_vg_cartao,8) \
          eval(let l_tpvenda = cartao_prop_loc.tp_venda)
ENDTRANSACTION
if {l_tpvenda,""} = "C"
let vg_gara=$messagebox($concat("                     ATENCAO!\a\a", \
        "1. O Cartao Paraiba do Cliente Encontra-se na Loja\a",\
        "2. Entregar ao titular!"),3,1,1,"N.C.P")
endif
Endproc

%-----------------------
localprocedure rot_PORCONTA(dt_venc2)
%-----------------------

    let  vflag= "0"
    if   {frecibo2.Vl_Total_Inf,0.00} = 0.00
         let vvalor = frecibo2.vl_Prestacao  dt_venc2 = frecibo2.Dt_Vencimento
         CALCJURO2(dt_venc2,Data1,vvalor)
         let  frecibo2.Vl_Recibo     = frecibo2.Vl_Prestacao \
              frecibo2.vl_desc_Inf      = 0                  \
              frecibo2.vl_desc_inf_juro = 0                  \
              frecibo2.Vl_Desconto      = 0                  \
              frecibo2.vl_juros      = vvalor                \
               frecibo2.vl_juros_real = vvalor                \
              frecibo2.Vl_Total_Rec  = (frecibo2.Vl_Recibo + \
              {frecibo2.Vl_Juros,0}) - {frecibo2.Vl_Desconto,0}
    else
         let  frecibo2.tp_recibo = 03

    if   {frecibo2.Nr_DiasJur_Inf,0.00} > 0.00
         let  vg_vlinf = frecibo2.vl_total_Inf \
              vg_dtven = frecibo2.Dt_Vencimento \
              vg_dtpag = Data1 \
              vg_vlmul = 0.00 \
              vg_vljur = 0.00

              if   vg_vlinf > 59999.00
              Mens("Estouro de Variavel!",12,20)
              let vflag = "1"
                   return
              endif

              CALJURA()

         if   vg_vlcal > frecibo2.vl_prestacao or vg_vlcal < 0.00
         Mens("Vlr Amortizacao Maior que Prestacao!",12,20)
         let frecibo2.vl_total_inf = 0.00
         form set (cursor) frecibo2.Vl_Total_Inf
         let vflag = "1"
                   return
              endif

              let  frecibo2.Vl_Recibo      = vg_vlcal \
                   frecibo2.vl_desconto    = 0 \
              frecibo2.Vl_Juros       = (vg_vlJur + vg_vlmul) \
              frecibo2.Vl_Juros_Real  = frecibo2.Vl_Juros
         else
         if   frecibo2.vl_total_Inf > frecibo2.vl_prestacao or \
              frecibo2.vl_total_inf < 0.00
         Mens("Vlr Amortizacao Maior que Prestacao!",12,20)
              let frecibo2.vl_total_inf = 0.00
              form set (cursor) frecibo2.Vl_Total_Inf
         let vflag = "1"
                   return
              endif

              let  frecibo2.Vl_Recibo    = frecibo2.vl_total_Inf \
         frecibo2.vl_juros     = 0.00   \
         frecibo2.vl_Desconto  = 0.00
         endif

         let frecibo2.Vl_Total_Rec   = frecibo2.Vl_Total_Inf

    endif

    if   vg_uf = " 2" and frecibo2.vl_recibo = frecibo2.vl_prestacao
         let frecibo2.tp_recibo = 02
    endif

    let  frecibo2.tp_rec = {"PRE" wh frecibo2.tp_recibo = "02","RES"}

    if   frecibo2.tp_recibo = " 3" and    \
         frecibo2.vl_recibo <> frecibo2.vl_prestacao
         let frecibo2.tp_rec = "P/C"
    endif

    let  frecibo2.Vl_Desc_Inf      = 0.00 \
    frecibo2.Vl_Desc_Inf_juro = 0.00 \
    vvalor                    = frecibo2.Vl_Juros

    form display noprompt

endprocedure

%-----------------------
localprocedure CALEND() Local(l_Fl_Util_Fer, \
                              l_Dt_Proximo_Util, \
                              l_No_Feriado)
%-----------------------

    transaction read
    Compute 1 calendario wh dt_calendario = frecibo2.Dt_Vencimento \
           Eval(Let l_Fl_Util_Fer = Calendario.Fl_Util_Fer)         \
               (Let l_Dt_Proximo_Util = Calendario.Dt_Proximo_Util) \
               (Let l_No_Feriado      = Calendario.No_Feriado)
    endtransaction
    if l_Fl_Util_Fer = "F" and $ToDate(l_Dt_Proximo_Util) = vg_Dtloja
       let frecibo2.Nr_DiasJur_inf = vg_Dtloja - $ToDate(l_Dt_Proximo_Util) \
           frecibo2.No_Feriado     =  l_No_Feriado
       form set (reverse foreground black) frecibo2.No_Feriado
    else
       let frecibo2.Nr_DiasJur_inf = frecibo2.Nr_DiasJur_Cal
       let frecibo2.No_Feriado     = " "
       form set (normal  foreground 8) frecibo2.No_Feriado
    endif

endprocedure

%-----------------------
localprocedure CRITICA()
%-----------------------

    let vflag = "0"

    if {frecibo2.cd_cliente,0} = 0
       Mens("ERRO - Verifique Cliente",12,25)
       form set (cursor) frecibo2.nr_contrato
       let vflag = "1"
       return
    endif

    if {frecibo2.Vl_Total_inf,0.00} > 0.00
       if {frecibo2.vl_Desc_Inf,0.00} > 0.00
           Mens("Desconto Invalido Para P/C!",12,25)
          let frecibo2.vl_desc_Inf = 0.00
     form set (cursor) frecibo2.vl_desc_inf
          let vflag = "1"
          return
       endif
       if frecibo2.vl_desc_inf_juro > frecibo2.vl_total_inf
          Mens("Juros maior que Amortizacao!",12,25)
          let frecibo2.vl_desc_Inf_juro =\
                   (frecibo2.vl_total_rec - frecibo2.vl_Recibo)
          let frecibo2.vl_juros     =  frecibo2.vl_desc_inf_juro \
              n1                    =  frecibo2.vl_desc_inf_juro
          form set (cursor) frecibo2.vl_desc_inf_juro
          let vflag = "1"
          return
      endif
    endif
    if {frecibo2.Vl_Total_inf,0.00} > 0.00 and \
       {frecibo2.vl_total_inf,0.00} < 2.00
       Mens("Valor minimo p/ recibo P/C eh de R$ 2.00",12,25)
          %let frecibo2.vl_total_inf = 0.00
     form set (cursor) frecibo2.vl_total_inf
          let vflag = "1"
          return
    endif

    if n1 <> frecibo2.vl_desc_inf_juro
       let frecibo2.vl_juros     =  frecibo2.vl_desc_inf_juro
       let n1                    =  frecibo2.vl_desc_inf_juro
    endif

    if   {frecibo2.vl_Desc_Inf,0.00} >= \
                     {frecibo2.vl_Recibo,0.00}
         Mens("Desconto maior que Valor do Saldo!",12,25)
         let frecibo2.vl_desc_Inf  = 0.00
         form set (cursor) frecibo2.vl_desc_inf
         let vflag = "1"
    endif

   if (frecibo2.vl_desc_inf>0 and frecibo2.vl_desc_inf_juro>0)  or \
         (frecibo2.vl_juros > 0 and frecibo2.vl_desconto > 0) or \
             (frecibo2.vl_desc_inf > 0 and frecibo2.vl_juros > 0)
      mens("Impossivel dar Desconto com Juros!",15,15)
      let frecibo2.vl_desc_Inf  = 0.00
      form set (cursor) frecibo2.vl_desc_inf
      let vflag = "1"
   endif

    let frecibo2.Vl_Desconto  =  frecibo2.vl_desc_Inf

    if   {frecibo2.Vl_Total_inf,0.00}  >  0
         let frecibo2.Vl_Recibo      =   frecibo2.Vl_Total_Rec -   \
            {frecibo2.Vl_Juros,0.00} -  {frecibo2.Vl_Desconto,0.00}
    else
         let frecibo2.Vl_Total_Rec   =   frecibo2.Vl_Recibo +      \
            {frecibo2.Vl_Juros,0.00} -  {frecibo2.Vl_Desconto,0.00}
    endif

    form display noprompt

endprocedure

%--------------------------------------------
localprocedure LIMPA(vflag) local (guarFin)
%--------------------------------------------

    let guarFin              = frecibo2.Tp_Fin
    form clear frecibo2
    let frecibo2.Sg_Loja     = vg_Loja            \
        frecibo2.Sg_Loja_Rec = vg_loja            \
        frecibo2.nr_tab      = "2"                \
        frecibo2.dt_recibo   = vg_dtloja
    let Data1                = frecibo2.Dt_Recibo
    let va_tabpre            = "GP2"
    form set (protected)   frecibo2
    form set (unprotected) frecibo2.Sg_loja_rec   \
                           frecibo2.Tp_Fat        \
                 \         frecibo2.nr_tab        \
               frecibo2.Nr_Contrato
    let frecibo2.Tp_Fin      = guarFin

   if vflag = "1"
      form set (cursor)   frecibo2.Nr_Contrato
   else
      form set (cursor)   frecibo2.Tp_Fat
   endif

endprocedure

%--------------------------------------------
LocalProcedure ROTPRE01(in var3,out dt_venc2) \
     Local(l_DtCanc,l_DtExclui,l_DtVenc)
%--------------------------------------------

    let vflag = "0" vg_Uf = " "
    transaction read
    find 1 Prestacao wh prestacao.cd_chave = $ToChar(var3,17) \
           Eval (Let l_DtCanc = {Prestacao.Dt_Cancel,Dt_Zero}) \
                (Let l_DtExclui = {Prestacao.Dt_Exclui,Dt_Zero}) \
                (Let l_DtVenc                = {Prestacao.Dt_Vencimento,Dt_Zero}) \
                (Let frecibo2.Vl_Prestacao   = Prestacao.Vl_Saldo_crs)     \
                (Let frecibo2.Nr_Contrato    = Prestacao.Nr_Contrato)      \
                (Let frecibo2.sg_loja_rec    = Prestacao.Sg_Loja)          \
                (Let frecibo2.Tp_Fin         = {Prestacao.Tp_Fin  wh       \
                                     {Prestacao.Tp_Fin_Rel," "} = " ",     \
                                             Prestacao.Tp_Fin_Rel})        \
                (Let frecibo2.Tp_Fat         = Vg_TpFat[frecibo2.Tp_Fin])  \
                (Let frecibo2.Tp_Fin_Rel     = {Prestacao.Tp_Fin Wh        \
                              {Prestacao.Tp_Fin_Rel," "} <> Prestacao.Tp_Fin, \
                                                     Prestacao.Tp_Fin_Rel}) \
                (Let frecibo2.Nr_Prestacao   = Prestacao.Nr_Prestacao)     \
                (Let frecibo2.Dt_Vencimento  = Prestacao.Dt_Vencimento)    \
                (Let frecibo2.Qt_Prestacao   = Prestacao.Qt_Prestacao)     \
                (Let frecibo2.Cd_Representante = Prestacao.Cd_Representante)    \
                (Let frecibo2.Cd_Divisao     = Prestacao.Cd_Divisao)            \
                (Let frecibo2.Tp_Recibo      = {03 wh Prestacao.Vl_Saldo_Crs <> \
                          Prestacao.Vl_Prestacao,02}) -> spre2
    endtransaction
    if $ToDate(l_DtCanc) > 0
       form display noprompt bell
       mens("Esta Prestacao esta Cancelada!",15,30)
       let vflag = "1"
       return
    endif

    if $ToDate(l_DtExclui) > 0
       form display noprompt bell
       mens("PRESTACAO EXCLUIDA!",15,30)
       let vflag = "1"
       return
    endif

    if $ToDate(l_DtVenc) < 19570101
       Mens("Vencimento Inferior a 01/01/1957!",15,30)
       let vflag = "1"
       return
    endif

    %-let frecibo2.Vl_Prestacao    = Prestacao.Vl_Saldo_crs

    if {frecibo2.Vl_Prestacao,0.00} = 0.00
       Mens("Prestacoes ja Estao Pagas!",15,30)
       let vflag = "1"
       return
    endif

        %---frecibo2.Tp_Fin           = \
         %---{Prestacao.Tp_Fin  wh {Prestacao.Tp_Fin_Rel," "} = " " Or     \
             %---(Prestacao.Tp_Fin = " 3" And Prestacao.Tp_Fin_Rel = " 2"),\
          %---Prestacao.Tp_Fin_Rel}                                 \

    let frecibo2.No_Cliente       = $substring(vg_nome,9,45)    \
   frecibo2.Cd_Cliente       = $substring(vg_nome,1,8)     \
   frecibo2.vl_desconto      = 0.00                        \
        frecibo2.Nr_Minuta        = vg_Minuta                   \
        %--frecibo2.Nr_Contrato      = Prestacao.Nr_Contrato       \
        %--frecibo2.sg_loja_rec      = Prestacao.Sg_Loja           \
        %--frecibo2.Tp_Fin           = {Prestacao.Tp_Fin  wh       \
        %--    {Prestacao.Tp_Fin_Rel," "} = " ",Prestacao.Tp_Fin_Rel} \
        %--frecibo2.Tp_Fat           = Vg_TpFat[frecibo2.Tp_Fin]   \
        %--frecibo2.Tp_Fin_Rel       = {Prestacao.Tp_Fin Wh        \
        %--                      {Prestacao.Tp_Fin_Rel," "} <>     \
        %--   Prestacao.Tp_Fin,Prestacao.Tp_Fin_Rel}               \
        %--frecibo2.Nr_Prestacao     = Prestacao.Nr_Prestacao      \
        %--frecibo2.Dt_Vencimento    = Prestacao.Dt_Vencimento     \
        %--frecibo2.Qt_Prestacao     = Prestacao.Qt_Prestacao      \
        %--frecibo2.Cd_Representante = Prestacao.Cd_Representante  \
        %--frecibo2.Cd_Divisao       = Prestacao.Cd_Divisao        \

    if $ToDate(l_DtVenc) < vg_Dtloja  %% CAL. DIAS DE ATRASO %%
       let frecibo2.Nr_Diasjur_Cal= vg_Dtloja - $ToDate(l_DtVenc)
    endif

    l_AON('1')
    Set CurrentSet sPre2

    let frecibo2.Vl_Prestacao = frecibo2.Vl_Prestacao + Vg_VlConta[98] \
        frecibo2.vl_recibo    = frecibo2.vl_prestacao            \
        %-frecibo2.Tp_Recibo    = {03 wh Prestacao.Vl_Saldo_Crs <> \
        %-                 Prestacao.Vl_Prestacao,02}

    if $ToDate(l_DtVenc) >= vg_Dtloja  %% DESCONTO P/ CLIENTE %%
       transaction read
       compute 1 loja wh sg_loja = vg_loja and loja.Vl_Perc_Desconto <> 0 \
                     eval(let frecibo2.Vl_Desconto = \
            frecibo2.vl_prestacao * loja.Vl_Perc_Desconto / 100)
       endtransaction
       if $membercount > 0
          let vg_descant = frecibo2.Vl_Desconto
       endif
    endif

    let vg_Uf = frecibo2.tp_Recibo

    CALEND()

    let vvalor = (frecibo2.vl_recibo - Vg_VlConta[98]) \
        dt_venc2 = frecibo2.Dt_Vencimento
    CALCJURO2(dt_venc2,Data1,vvalor)
    let frecibo2.nr_diasjur_inf = {vg_diafin,0} \
        vvalor = vvalor + Vg_VlConta[99]

    if   {frecibo2.vl_desc_inf_juro,0}  > vvalor
         let  frecibo2.vl_desc_inf_juro  = 0
    endif
    let frecibo2.vl_juros      = vvalor  -  {frecibo2.vl_desc_inf_juro,0}  \
   frecibo2.vl_juros_real = vvalor

    let frecibo2.Vl_Total_Rec = (frecibo2.Vl_Recibo +\
           {frecibo2.Vl_Juros,0}) - {frecibo2.Vl_Desconto,0}

    %% Arrerec(chvarre,2)  %% CALCULA ARREDONDAMENTO DA LOJA %%%

    if {frecibo2.vl_prestacao,0} <> 0 %% CASO EM ABERTO
    %  If {Fl_Caixa, " "} = "I"
    %     Form Set (UnProtected) fRecibo2.Vl_Total_Inf
    %  Else
          form set (protected) frecibo2.Vl_Desc_Inf frecibo2.vl_desc_inf_juro
          let fcxautor.cd_autoriza = 0 Cd_Mat_Senha = " " No_Usu_Senha = " "
          form set (unprotected) frecibo2.Dt_Cheque frecibo2.Vl_Total_Inf
    %  EndIf

    else %% CASO LIQUIDADA %%

       form set (protected) frecibo2.Vl_Desc_Inf frecibo2.Vl_Total_Inf   \
                            frecibo2.Dt_Cheque frecibo2.vl_desc_inf_juro
    endif

    let frecibo2.tp_rec = {"PRE" wh frecibo2.tp_recibo = "02","RES"}
    form set (cursor) frecibo2.Vl_Total_Inf

    Rot_ReciboEmitido()

    form clear frecibo2.Dt_Cheque

EndProcedure
LocalProcedure Rot_ReciboEmitido()

    Let Vg_Es ="N"
    transaction read
    Compute 1 recibo wh cd_chave_pre = $concat(frecibo2.Sg_Loja_Rec," 2",\
               frecibo2.Tp_Fin,frecibo2.Nr_Contrato,frecibo2.Nr_Prestacao) \
          and recibo.dt_recibo =  frecibo2.Dt_Recibo \
          and recibo.dt_caixa Is $Null and dt_cancel Is $Null  \
          and recibo.tp_recibo not in (" 3"," 4") eval(1)
    endtransaction
    if $membercount > 0
       Let Vg_Es ="S"
       form display noprompt bell
       mens("Recibo Ja Foi Emitido P/ Esta Prestacao!!",12,20)
       If {Fl_Caixa, " "} <> "I"
          form set (unprotected) frecibo2.Nr_Prestacao
          form set (cursor) frecibo2.Nr_Prestacao
       EndIf
    else
       form set (protected) frecibo2.Nr_Prestacao
    endif

EndProcedure

%------------------------------------------------------
LocalProcedure ROTCHEQUE(inout dataant,in vpreant) local(dt_venc2)
%------------------------------------------------------

    let vflag = "0"
    if  {frecibo2.Dt_Cheque,0} = 0
        let data1 = frecibo2.Dt_Recibo
        if  frecibo2.Dt_Vencimento < data1  %% CAL. DIAS DE ATRASO %%
            let frecibo2.Nr_Diasjur_Cal= data1 - frecibo2.Dt_vencimento
        endif

        CALEND()

    else
        if  {frecibo2.Dt_Cheque,0} <= {frecibo2.Dt_Recibo,0}
       Mens("Data de Cheque Pre-Datado Invalida!",12,20)
            let frecibo2.Dt_cheque = dataant
       form set (cursor) frecibo2.Dt_Cheque
            let vflag = "1"
            return
        endif

        %% CHECAGEM DA DATA DE PAGAMENTO %%
        transaction read
        Compute 1 JurAtraso wh Cd_Chave = \
                            $concat(va_tabpre,frecibo2.Dt_Cheque)  eval(1)
   endtransaction
        if  $membercount = 0
            form display bell noprompt
            mens("DT.PAGAMENTO SEM TAXA DE JUROS!",15,30)
            let frecibo2.Dt_cheque = dataant
       form set (cursor) frecibo2.Dt_Cheque
            let vflag = "1"
            return
        endif

        let Data1 = frecibo2.Dt_Cheque
        if  frecibo2.Dt_Vencimento < data1  %% CAL. DIAS DE ATRASO %%
            let frecibo2.Nr_Diasjur_Cal= data1 - frecibo2.Dt_vencimento \
                frecibo2.Nr_Diasjur_Inf= frecibo2.nr_diasjur_Cal
        else
            let frecibo2.Nr_DiasJur_inf = 0 frecibo2.Nr_DiasJur_Cal = 0
        endif
    endif

    let vvalor = frecibo2.vl_recibo   dt_venc2 = frecibo2.Dt_Vencimento

    if   {frecibo2.vl_total_inf,0} <> vPreAnt  or  \
         {frecibo2.vl_total_inf,0} <> 0
         rot_PORCONTA(dt_venc2)
    if   vflag = "1"
              return
         else
              let  vpreant = frecibo2.vl_total_Inf
         endif
    else
         CALCJURO2(dt_venc2,Data1,vvalor)
         if   frecibo2.vl_desc_inf_juro  > vvalor
              let  frecibo2.vl_desc_inf_juro  = 0
         endif
         let  frecibo2.vl_juros      = vvalor  -  frecibo2.vl_desc_inf_juro  \
              frecibo2.vl_juros_real = vvalor \
              frecibo2.Vl_Total_Rec  = (frecibo2.Vl_Recibo +\
                               {frecibo2.Vl_Juros,0}) - {frecibo2.Vl_Desconto,0}
    endif

    let DataAnt = frecibo2.Dt_cheque

EndProcedure

LocalProcedure Rot_DiasAtraso ()  local (vdiasa)


   %% CLIENTE COM XXX DIAS EM ATRASO (padrao 180 dias)
   Let vdiasa = 0  vBc_Dforc ="N"
   Compute 1 ConfigLoja Wh Cd_Chave = $Concat(frecibo2.Sg_Loja,"9904") \
           and Fl_Conf = "S" Eval (let vdiasa = nr_confn)
   If $MemberCount > 0

      If {vdiasa,0} = 0
         Mens2($concat("Conf. 9904 - Falta Informar Qt. Dias Atraso P/", \
              " Encaminhar A Cobranca!"),15,15)

      elseIf ($date - frecibo2.dt_vencimento) > vdiasa
         Let vBc_DForc ="S"
         Mens2 ($concat("Atencao! Cliente Deve Ser Encaminhado Ao Setor", \
               " de Cobranca!"),15,30)
      EndIf

   EndIf

EndProcedure
%--------------------------------------------------------------
%                     INICIO DO GREC
%--------------------------------------------------------------

procedure Grec() local(valor,var3,Dt_venc2,seq,chvant,chave_pre,desconto_pre, \
                       chvarre,ParImp,prestant,vpreAnt,DataAnt,vST,  \
                       vDt, vDV, vDI, vDF, vPc,vNCP, vAutoriza,      \
                       l_DtCanc,l_DtExclui,l_DtVenc)
ON DEADLOCK
   QUITTRANSACTION
   Mens("Conflito, Tente Novamente",10,10)
   goto ROTINPUT
ENDON

%                 PERMISSAO             %
set information off
transaction read
find all loja wh Sg_Loja = vg_loja eval               \
    (let vg_Dtloja = dt_loja) (let vg_dtcontr1 = loja.dt_control1)
endtransaction
win open wpadrao for frecibo2
form select frecibo2
form set transmit f8
form set exit escape f1 f2 f3 f4 f5 f6 f7 f9 f10 f12
let vflag = "0" vg_clirec = " " vpreAnt = 0.00 vtpmin =" " DataAnt = " " \
    vg_diafin = 0 chave = " " n1 = 0  vDV = 0  vDI = 0  vDF = 0  vPc = 0 vDt=0 \
    desconto_pre=0
Compute ConfigLoja wh Cd_Chave = $Concat(Vg_Loja,"6438") and        \
        {Fl_Conf," "} = "S" Eval     (Let vDV = {Dt_Inclusao,0})    \
        (Let vDI = {Dt_Inicio,0})    (Let vDF = {Dt_Encerra,0})     \
        (Let vPc = {Nr_ConfN, 0})

Let vNCP  = "N"
Compute 1 ConfigLoja wh Cd_Chave = $Concat(Vg_Loja,"3001") Eval  \
        (Let vNCP  = {ConfigLoja.Fl_Conf, " "})

%-----------------------------------Retira Mensagem Dos Campos (RADIO)
If Vg_Regiao = "10"
   Form Set (Invisible) fRecibo2.Msg
EndIf

form clear frecibo2
form display

form set (REVERSE) cursor
form set (transmit)   \
         frecibo2.nr_contrato frecibo2.vl_desc_Inf frecibo2.vl_desc_inf_juro  \
         frecibo2.vl_total_Inf frecibo2.Nr_Prestacao
let  valor = 0    chvarre = 0

LIMPA("0")
let va_tabpre       = "GP2"

if vg_cliRec = "S"
   transaction read
   Find 1 prestacao wh cd_chave_con = Chave_Con and vl_saldo_Crs > 0 \
         eval(let var3=cd_chave) (let frecibo2.Nr_Contrato = Nr_Contrato)\
             (Let frecibo2.Sg_Loja_Rec = Sg_Loja)                        \
             (Let frecibo2.Tp_Fin = Tp_Fin)                              \
             (Let frecibo2.Tp_Fat = Vg_TpFat[Tp_Fin])                    \
             (Let frecibo2.Nr_Prestacao = Nr_Prestacao)
   endtransaction
   if $setcount = 0
      Mens("Nao ha prestacoes abertas",10,5)
      Return
   Endif
   Goto PesqCli
Endif

If {Fl_Caixa, " "} = "I"
   Let vSt = "N"
   Compute 1 ConfigLoja wh Cd_Chave = "SEV   1" Eval   \
           (Let vSt = ConfigLoja.Fl_Conf)
   If vSt = "S"
      Let vSt = "S"
      Conecta ("V")
      If $LastErrCode > 0 or $SqlErrCode > 0
         Mens ("PROBLEMA NA CONEXAO. COMUNIQUE AO C.P.D.",15,30)
         Let vSt = "N"
      EndIf
   Else
      Let vSt = "S"
   EndIf

Endif
<<
If {Fl_Caixa, " "} = "I"
   Form Set (Protected) fRecibo2.Sg_Loja_Rec       fRecibo2.Tp_Fat       \
                        fRecibo2.Nr_Contrato       fRecibo2.Dt_Cheque    \
                        fRecibo2.Vl_Desc_Inf_Juro  fRecibo2.Vl_Desc_Inf  \
                        fRecibo2.Nr_Prestacao
EndIF
>>

form display noprompt
if chave_con <> " "
   let frecibo2.Sg_Loja     = vg_loja
   let frecibo2.Sg_Loja_rec = $substring(chave_con,1,3)
   let frecibo2.tp_fin      = $substring(chave_con,6,2)
   let frecibo2.tp_fat      = Vg_TpFat[frecibo2.tp_fin]
   let frecibo2.nr_contrato = $substring(chave_con,8,8)

   If frecibo2.Sg_Loja_Rec = 'AON'
      l_AON('2')
      If frecibo2.Sg_Loja_Rec = ' '
         Win Close wPadrao
         Return
      EndIf
   EndIf
   goto PrepREC
endif

WHILE
checa_tp_venda(frecibo2.cd_cliente)
ROTINPUT:
    form display input noprompt
  GUtils ()
  if  $transmitkey in ("F8","F9") and $trim(DataAnt) <> {frecibo2.Dt_Cheque," "}
      RotCheque(DataAnt,vpreant)
      if  vflag = "1"
          continue
      endif
  endif
  case
    when $transmitkey in  ("Escape","f3")  % RETORNAR %
         If {Fl_Caixa, " "} = "I"
            Let Vg_Defe = "NAO"
         EndIf
         break

    when $transmitkey ="F1" and {Fl_Caixa," "} = "I"
         Calcu()

    when $transmitkey ="f6" and {Fl_Caixa," "} <> "I" % ROTINA DE CONSULTA %
          CREC()

    %--------------------- PESQUISAS(NOME CONT PREST - PREENCHE RECIBO)
    when $transmitkey = "F2" and {Fl_Caixa, " "} <> "I"
         $preccli(var3,"3"," ")  %% PICK_LIST PARA CONSULTA DE CLIENTES %%

         if var3 <> volta  %% CASO TENHA ESCOLHIDO %%
            let vg_descant = 0   vg_juroant = 0
       LIMPA("0")

            PesqCli:
       let DataAnt = " "
       let Data1   = frecibo2.Dt_Recibo
       ROTPRE01(var3,dt_venc2)
       if vflag = "1"
               if vg_clirec="S"
                  return
               endif
            endif
            let chvant = $concat(frecibo2.Sg_Loja_Rec," 2",\
            frecibo2.Tp_Fin,frecibo2.Nr_Contrato) \
                vPreAnt= 0.00
       let prestant = frecibo2.Nr_Prestacao
         endif
         form display noprompt

    when $fieldnum  = 900 and $transmitkey not in ("f4","f8")
         If {fRecibo2.Tp_Fat,"  "} in ("IR","II","IV") and vNCP ="S"
            Mens2($Concat("FATURAMENTO N.C.P , USE O PROGRAMA",  \
                  " GRECNCP - EMITE RECIBO N.C.P !"),15,30)
            form clear frecibo2.Tp_Fat
            form set (cursor) frecibo2.Tp_Fat
            form display noprompt bell
            continue
         EndIf

           let frecibo2.tp_fin = 0
           compute 1 financeiro wh cd_chave_fat = \
       $concat(frecibo2.Sg_loja,frecibo2.Tp_Fat) \
             eval(let frecibo2.tp_fin = financeiro.tp_fin)
         If (Vg_Anexo = "S" And $IsOdd(fRecibo2.Tp_Fin) = 0)
            Mens("FATURAMENTO INEXISTENTE!",15,30)
            form clear frecibo2.Tp_Fat
            form set (cursor) frecibo2.Tp_Fat
            form display noprompt bell
            continue
        EndIf
        if frecibo2.tp_fin <> "14"
           transaction read
           compute 1 financeiro wh cd_chave = \
       $concat(frecibo2.Sg_loja,frecibo2.Tp_Fin) eval(1)
           endtransaction
           if $membercount = 0 Or \
              (Vg_Anexo = "S" And $IsOdd(fRecibo2.Tp_Fin) = 0)
              Mens("FATURAMENTO INEXISTENTE!",15,30)
              form clear frecibo2.Tp_Fat
              form set (cursor) frecibo2.Tp_Fat
              form display noprompt bell
              continue
           endif
        endif

    when $transmitkey = "F5" and {Fl_Caixa," "} <> "I"  % Alt. Nr. Prestacao %
         form set (unprotected) frecibo2.nr_prestacao
         form set (cursor) frecibo2.nr_prestacao
    let prestant = " "
         continue

    when $transmitkey = "F4"  % ENTRADA / OUTRAS LOJAS   %
    if {vg_dtcontr1,0} > 0
          if {frecibo2.dt_recibo,0} <= vg_dtcontr1
                mens("ATENCAO: ESTE MES ESTA FECHADO!",15,30)
          LIMPA("0")
                continue
          endif
    endif
    IrecEnt()
         form set (normal  foreground 8) frecibo2.No_Feriado
    LIMPA("0")

    when $transmitkey = "f8"      %%%% ROTINA DE INCLUSAO %%%%
    if {frecibo2.Nr_Prestacao,0} = 0
            Mens("Falta Informar Num. da Prestacao!",15,30)
       continue
         endif
    if {frecibo2.tp_Recibo,0} = 0
       continue
         elseif {frecibo2.vl_Recibo,0.00} <= 0.00
            Mens("Vlr. Amortizacao nao pode ser Zero.",15,30)
       continue
         endif

         if   {frecibo2.vl_total_inf,0} <> vPreAnt
              rot_PORCONTA(dt_venc2)
         if   vflag = "1"
                   continue
              else
                   let  vpreant = frecibo2.vl_total_Inf
              endif
         endif

         if  chvant <> $concat(frecibo2.Sg_Loja_Rec," 2",\
                             {frecibo2.Tp_Fin," "},frecibo2.Nr_Contrato) or\
                {Prestant," "} <> frecibo2.Nr_Prestacao
             let chvant = $concat(frecibo2.Sg_Loja_Rec," 2",\
            frecibo2.Tp_Fin,frecibo2.Nr_Contrato)

             %% IF somente para CAM                                     %%
             %% Caso Possua Reps devera ser not in (rep1,rep2,...,repN) %%

             if  frecibo2.Sg_Loja  <>  frecibo2.sg_loja_rec
               transaction read
                 Compute 1 Contrato wh Contrato.cd_chave_con =\
                frecibo2.Sg_Loja_Rec? eval(1)
               endtransaction
                 if  $membercount = 0
                     mens("Use F4 p/ Outra Loja!",15,30)
                 Endif
                 Continue
             endif

             let vg_descant = 0 vg_juroant = 0

        let vg_ac = frecibo2.tp_fin
        let frecibo2.tp_fin = {"13" wh vg_ac = "14",  \
                                    " 4" wh vg_ac = " 5",vg_ac}
           transaction read
             find all Prestacao wh prestacao.cd_chave_con =\
                     $concat(frecibo2.Sg_Loja_Rec," 2",\
                             frecibo2.Tp_Fin,$ToChar(frecibo2.Nr_Contrato,8)) \
                 sorted by nr_prestacao           \
                 Eval(Let l_DtCanc = {Prestacao.Dt_Cancel,Dt_Zero})   \
                     (Let l_DtExclui = {Prestacao.Dt_Exclui,Dt_Zero})       \
                (Let frecibo2.Cd_Cliente       = prestacao.Cd_Cliente) \
                     (Let frecibo2.Cd_Representante = prestacao.Cd_Representante) \
                     (Let frecibo2.Cd_Divisao     = Prestacao.Cd_Divisao)   \
                     (Let frecibo2.Qt_Prestacao   = Prestacao.Qt_Prestacao) \
                     (Let vg_minuta               = Prestacao.Nr_Minuta)    \
           (Let vtpmin                  = prestacao.tp_minuta) -> spre2
           endtransaction
             if $setcount < 1 Or \
                (Vg_Anexo = "S" And $IsOdd(fRecibo2.Tp_Fin) = 0)
           mens("CONTRATO INEXISTENTE!",15,30)
                form set (normal  background 8) frecibo2.No_Feriado
                let chvant = $concat(frecibo2.Sg_Loja_Rec," 2",\
                  frecibo2.Tp_Fin,frecibo2.Nr_Contrato)
                LIMPA("1")

           let frecibo2.Nr_Contrato = $substring(chvant,08,8) \
              frecibo2.Sg_Loja_Rec = $substring(chvant,01,3)\
              frecibo2.Tp_Fin      = $substring(chvant,06,2)\
                        frecibo2.Tp_Fat = Vg_TpFat[frecibo2.Tp_Fin]

           form display noprompt
           Continue
           endif

        if $ToDate(l_DtCanc) > 0
           form display noprompt bell
           mens("CONTRATO CANCELADO!",15,30)
             endif

        if $ToDate(l_DtExclui) > 0
           form display noprompt bell
           mens("CONTRATO  EXCLUIDA!",15,30)
                form set (normal  background 8) frecibo2.No_Feriado
                let chvant = $concat(frecibo2.Sg_Loja_Rec," 2",\
                                 frecibo2.Tp_Fin,frecibo2.Nr_Contrato)
                LIMPA("1")

           let frecibo2.Nr_Contrato = $substring(chvant,08,8)\
               frecibo2.Sg_Loja_Rec = $substring(chvant,01,3)\
               frecibo2.Tp_Fin      = $substring(chvant,06,2)\
                    frecibo2.Tp_Fat      = Vg_TpFat[frecibo2.Tp_Fin]

                form set (cursor) frecibo2.Tp_Fat
           form display noprompt
           Continue
             endif

        %-let frecibo2.Cd_Cliente       = prestacao.Cd_Cliente \
             %-    frecibo2.Cd_Representante = prestacao.Cd_Representante \
             %-    frecibo2.Cd_Divisao       = Prestacao.Cd_Divisao \
             %-    vg_minuta               = Prestacao.Nr_Minuta   \
             %-       vtpmin                  = prestacao.tp_minuta

             %% LEITURA DO NOME DO CLIENTE
           transaction read
             compute 1 cliente wh cd_chave = \
                $concat(Vg_Regiao,frecibo2.Cd_Cliente)\
                     eval(let vg_nome = $concat(Cd_Cliente,Cliente.No_Cliente))
           endtransaction
             if $membercount = 0
           form display noprompt bell
           mens("ATENCAO!!! Verifique o Cliente.",15,30)
             endif

             if {Prestant," "} <> frecibo2.Nr_Prestacao
           if frecibo2.Nr_prestacao > {frecibo2.Qt_Prestacao,0}
         Mens("Prestacao Inexistente!",12,20)
         form clear frecibo2.nr_prestacao
         form set (cursor) frecibo2.nr_prestacao
         continue
                elseif frecibo2.Nr_prestacao < Prestant
          Mens("Prestacao Ja Esta Paga!",12,20)
          form clear frecibo2.nr_prestacao
          form set (cursor) frecibo2.nr_prestacao
          continue
                endif
                let var3 = $concat(frecibo2.Sg_Loja_Rec," 2",\
                             frecibo2.Tp_Fin,frecibo2.Nr_Contrato,\
              frecibo2.Nr_Prestacao)
           let prestant = frecibo2.Nr_Prestacao
             else
                Compute 1 spre2 wh Prestacao.vl_saldo_crs <> 0.00 \
                   Eval(Let var3 = Prestacao.Cd_Chave)  \
                  (Let prestant = prestacao.nr_prestacao)
                %-let var3 = Prestacao.Cd_Chave
           %-let prestant = prestacao.nr_prestacao
             endif

        let DataAnt = " "
        let Data1   = frecibo2.Dt_Recibo

        ROTPRE01(var3,dt_venc2)
        continue
         endif

         CRITICA()
    if   vflag = "1"
              continue
         endif

        if vflag = "1"
                if vg_clirec="S"
                   return
                endif
             endif

             let chvant = $concat(frecibo2.Sg_Loja_Rec," 2",\
                              frecibo2.Tp_Fin,frecibo2.Nr_Contrato)

    if {vg_dtcontr1,0} > 0
          if {frecibo2.dt_recibo,0} <= vg_dtcontr1
                mens("ATENCAO: ESTE MES ESTA FECHADO!",15,30)
          LIMPA("0")
                continue
          endif
    endif

         Rot_DiasAtraso()

         Compute 1 ConfigLoja Wh Cd_Chave = $Concat(fRecibo2.Sg_Loja,"3014") \
              Eval("")
         If $MemberCount = 1
            Mens2("Utilize Programa No Computador Local (Login: acesso)",10,10)
     %      Continue
         EndIf

         conf("CONFIRMA INCLUSAO.")
         if  $fieldnum=1

             Rot_ReciboEmitido()
             If {Vg_ES," "} ="S"
                Continue
             EndIF

        GRAVA:
             if {frecibo2.tp_rec," "} in ("P/C","RES")
                let vg_nr_parc = 0  VICUB = " "
                let vicub = $concat(frecibo2.sg_loja_rec," 2",frecibo2.tp_fin,\
                          frecibo2.nr_contrato,frecibo2.nr_prestacao)

                find all porconta wh cd_chave_pre = VICUB -> spor_parc

                if $setcount = 0
                   let vg_nr_parc = 1
                else
                   find all spor_parc sorted by porconta.cd_chave -> spor_parc
                   BOT spor_parc
                   compute 1 spor_parc eval(let vg_nr_parc = porconta.nr_parcela+1)
                endif
             endif

             Let fRecibo2.Fl_ModoPagto =" "
             If frecibo2.Tp_Fin in (4,5,79,80) and Vg_Regiao = 10

                IniWait ("  CONTRATO DE REFINANCIAMENTO",14)
                GCxEsc (16,25,"PAGAR COM CARTAO DE CREDITO?","NAO","SIM")
                If $TransmitKey in ("Escape", "F3")
                   FimWait()
                   Continue
                EndIf
                FimWait()
                If $fieldnum = 2
                   Let fRecibo2.Fl_ModoPagto ="3"
                EndIf

             EndIf

        TRANSACTION
          set errors off
        %  set errors on
               compute 1 loja wh sg_loja = frecibo2.sg_loja \
                       eval(let Vg_Loja_Recibo = Loja.Sg_Loja_Recibo)
               if $membercount = 0 or {vg_loja_recibo," "} = " "
             QUITTRANSACTION
        Mens("Verifique loja-Recibo na tab. de Loja!",12,20)
                  continue
               endif

               let ParImp = {1 wh $isodd(frecibo2.Tp_Fin) = $true,2}
               ch 1 numerarec wh cd_chave = $concat(vg_loja_recibo,ParImp) \
          let  Nr_Recibo = Nr_Recibo + 1 \
          eval(let seq = Nr_Recibo)

          let $lasterrcode=0
          add Recibo from frecibo2\
         let Recibo.Sq_Imp_Par         = ParImp    \
             Recibo.Cd_Regiao          = Vg_Regiao \
             Recibo.Nr_recibo          = $abs(seq) \
             Recibo.Tp_Contrato        = " 2"      \
             Recibo.Tp_moeda           = fRecibo2.Tp_Fin_Rel \
             Recibo.Vl_prestacao_moeda = 0.00      \
             Recibo.Dt_Caixa           = 0         \
             Recibo.Vl_Recibo_moeda    = 0.00      \
             \Recibo.Cd_Mot_Desconto = {" " wh desconto_pre=0,'888'}\
             Recibo.Cd_Mot_Desconto = {" " wh vg_local_desc=N,'888'}\
             Recibo.Cd_Mot_DisJur      = " "       \
             Recibo.Cd_Autoriza        = fcxautor.cd_autoriza \
             Recibo.Nr_mesref          = " "       \
             Recibo.Nr_anoref          = " "       \
             Recibo.Dt_Cancel          = 0         \
             Recibo.Fl_situacao        = "2"       \
             Recibo.Vl_arredonda       = 0         \
             Recibo.tp_financ          = {frecibo2.cd_almoxarifado  \
                       wh {frecibo2.cd_almoxarifado,0} > 0,1}             \
             Recibo.Tp_Minuta          = vtpmin    \
                       Recibo.Tp_Recibo          =           \
                           {" 2" wh frecibo2.tp_rec = "PRE",\
                            " 3" wh frecibo2.tp_rec = "P/C",\
                            " 4" wh frecibo2.tp_rec = "RES"}       \
                       Recibo.Nr_Tab             = frecibo2.Nr_Tab \
                       Recibo.Nr_Minuta_Aut      = Cd_Mat_Senha    \
                       Recibo.No_Historico       = fChekSenha.Cd_Senha \
                       Recibo.Fl_ModoPagto       = fRecibo2.Fl_ModoPagto

              if $lasterrcode= 0
            ENDTRANSACTION
                 If $Left(fAcesso.Tec1,1) = "#"
                    System "" Close
                 EndIf
              else
            QUITTRANSACTION
                 if $lasterrcode = 8007
                    Mens2($concat("ATENCAO! Existe Recibo (",\
                           $toalpha($concat(Vg_Loja_recibo,ParImp,\
                            $rightjustify($toalpha(Seq,6))),10),\
                             "). Ele Sera Separado com tipo 8 ou 9."),12,20)
                    find 1 Recibo wh cd_chave = \
                             $concat(vg_loja_recibo,ParImp,\
                               $rightjustify($toalpha(Seq,6))) -> sRmuda
                    if Recibo.Dt_recibo =  vg_DtLoja
                       ch 1 numerarec wh cd_chave = \
                               $concat(vg_loja_recibo,ParImp) \
                     let  Nr_Recibo = Nr_Recibo + 1
                    else
                       Ch 1 sRmuda let Sq_Imp_par = {"8" wh ParImp = 2,"9"}
                    endif

                    Mens("CORRIGIDO! Tente Novamemte!",12,20)
                 else
                   Checa_Erro("001","Recibo",$lasterrcode)
                 endif
                 continue
              endif

              If {Fl_Caixa, " "} = "I"
                 Let Vg_Defe = $concat(ParImp,$toalpha($abs(seq),6))
                 Break
              EndIf

         set errors on
%-------------mens($concat("RECIBO DE NUMERO ",$trim(seq),"!"),15,30)
              form set (normal  foreground 8) frecibo2.No_Feriado

         set pause off
                let VG_AC = frecibo2.tp_Fin
                let frecibo2.Tp_Fin = {"13" wh VG_AC = "14", \
                                       " 4" wh vg_ac = " 5",VG_AC}
              transaction read
                Compute 1 Prestacao wh cd_chave = \
                $concat(frecibo2.Sg_Loja," 2",frecibo2.Tp_Fin,\
                   frecibo2.Nr_Contrato,frecibo2.Nr_Prestacao) \
                          eval(let valor  = Prestacao.vl_saldo_crs - \
                        frecibo2.vl_recibo)
      endtransaction
                if $membercount > 0
                   if valor < 0.01
                      let frecibo2.tp_recibo =  4
                   endif
                endif
              transaction read
         set out docimp %% IMPRESSAO HORIZONTAL  %%
                if  frecibo2.sg_loja in ("JSP","FMO","BAP","PAM","RPM")
          relrecibo2($concat(frecibo2.Sg_loja,\
                                       ParImp,$toalpha($abs(seq),6)))
                else
          relrecibo($concat(frecibo2.Sg_loja,\
                                      ParImp,$toalpha($abs(seq),6)),"N")
                endif
                let frecibo2.tp_Fin = VG_AC
         set out terminal
              endtransaction
                 If $Left(fAcesso.Tec1,1) = "#"
                    System "" Close
                 EndIf

         ROTIMP("GRECIBO")

         %%relrecibo2(var3,acres)   % IMPRESSAO VERTICAL %%

         Mens("Impressao Concluida.",15,30)
              LIMPA("0")
         form display noprompt
         endif

    when $transmitkey = "f7"   %%%%%%%% libera desconto %%%%%%%%%
         let vsenhaok="  "
         ChekSenha("DE"," ","S")
         If $pos(vg_classet,"DE") <> 0  or  vsenhaok = "DE"

            Let vAutoriza = 0
            If {frecibo2.vl_juros,0} <> {frecibo2.vl_juros_real,0} and \
               {fCxAutor.cd_autoriza,0} > 0
               Let vAutoriza = fcxAutor.Cd_autoriza
            Endif

            gInfAutor()     %------ Verifica Autorizante
            If {fCxAutor.Cd_Autoriza,0} = 0
               If {frecibo2.vl_juros,0} <> {frecibo2.vl_juros_real,0} and \
                  {fCxAutor.cd_autoriza,0} = 0
                  Let fcxAutor.Cd_autoriza = vAutoriza
               Endif

               Continue
            EndIf
            if frecibo2.dt_recibo >= frecibo2.dt_vencimento
               form set (unprotected) frecibo2.Vl_Desc_Inf               \
                                      frecibo2.Vl_Desc_Inf_Juro
            else
               opc ("Desconto Antecipado","Manual","Sistema",10,10)
               if $fieldnum = 1
                  Let vg_local_desc ="N"
                  form set (unprotected) frecibo2.Vl_Desc_Inf               \
                                         frecibo2.Vl_Desc_Inf_Juro
               elseif $fieldnum =2
                  ChekSenha("WQ"," ","S")
                  If $pos(vg_classet,"WQ") <> 0  or  vsenhaok = "WQ"
                     let chave_pre=$concat(                                 \
                         \frecibo2.sg_loja_rec," 2",frecibo2.tp_fin,         \
                         frecibo2.sg_loja_rec," 2",frecibo2.tp_fin_rel,     \
                         frecibo2.nr_contrato,frecibo2.nr_prestacao)        \
                         desconto_pre=0 vg_local_desc ="N"
                     rreducao(chave_pre,desconto_pre)
                     Let vg_juros = 0
                     if desconto_pre > 0
                        Let vg_local_desc ="S"
                        let frecibo2.vl_desc_inf=desconto_pre              \
                            frecibo2.vl_desconto=desconto_pre              \
                            frecibo2.Vl_Total_Rec=frecibo2.Vl_Recibo    +  \
                                                 {frecibo2.Vl_Juros,0}  -  \
                                                  desconto_pre
                     endif
                  endif
               endif
            endif
         EndIf
    continue

     %%%%    Arrerec(chvarre,1)  %% CALCULA ARREDONDAMENTO DA LOJA %%%

         % INVOCA TELA DE PARAMETROS %
    when $transmitkey = "f9" and {Fl_Caixa," "} <> "I"
    if {frecibo2.vl_recibo,0.00} = 0.00
       continue
    endif
         win open wpadrao  at 07 04 for fparamrec
         form select fparamrec
         form set transmit "Escape" "Return" "Enter"
    transaction read
         Find 1 loja wh Sg_Loja = Vg_Loja \
         eval(let  fparamrec.Nr_Dias_Atraso   = Loja.Nr_Dias_Atraso) \
             (let  fparamrec.Nr_Dias_Carencia = Loja.Nr_Dias_Carencia)  \
             (let  fparamrec.Vl_Perc_Atraso   = Loja.Vl_Perc_Atraso)
    endtransaction
         let fparamrec.Dt_Venc_Calc  = Dt_venc2 \
        fparamrec.Dt_Vencimento = frecibo2.Dt_Vencimento \
             fparamrec.Diasatraso    = vconta \
             fparamrec.Ind_Venc      = vg_tx1 \
             fparamrec.Ind_Pagto     = vg_tx2 \
             fparamrec.Juros_Calc    = vvalor

         form display input
         GUtils ()
         if $transmitkey in ("Escape","Return","Enter")
            win close wpadrao
         endif

    when $fieldnum  = 900  %% Tp_Fin  %% Tp_Fat
           let frecibo2.tp_fin = 0
         compute 1 financeiro wh cd_chave_fat = \
           $concat(frecibo2.Sg_loja,frecibo2.Tp_Fat) \
             eval(let frecibo2.tp_fin = financeiro.tp_fin)
        if frecibo2.tp_fin <> "14"
      transaction read
           compute 1 financeiro wh cd_chave = \
       $concat(frecibo2.Sg_loja,frecibo2.Tp_Fin) eval(1)
      endtransaction
           if $membercount = 0 Or \
              (Vg_Anexo = "S" And $IsOdd(fRecibo2.Tp_Fin) = 0)
              Mens("FATURAMENTO INEXISTENTE!",15,30)
              form clear frecibo2.Tp_Fat
              form set (cursor) frecibo2.Tp_Fat
              form display noprompt bell
              continue
           endif
        endif

    when $fieldnum  = 30 %% tipo tab juros
        if  {frecibo2.vl_desc_inf_juro,0}  >  0
            Mens("Se informar Juros Nao Muda de Tab",15,30)
            form set (cursor) frecibo2.vl_desc_inf_juro
            let frecibo2.nr_tab = "2"
            form display noprompt bell
            continue
        endif
        if frecibo2.nr_tab in ("1","2")
           if frecibo2.nr_tab = "1"
              %if frecibo2.nr_diasjur_cal < 241
              %   Mens("Tipo 1 soh para mais de 240 dias!",15,30)
              %   let frecibo2.nr_tab = "2"
              %   form set (cursor) frecibo2.nr_tab
              %   form display noprompt bell
              %   continue
              %endif
              let va_tabpre = "GP1"
           else
              let va_tabpre = "GP2"
           endif
           if   {frecibo2.vl_total_inf,0} <> vPreAnt  or  \
                {frecibo2.vl_total_inf,0} <> 0
                rot_PORCONTA(dt_venc2)
           if   vflag = "1"
                     return
                else
                     let  vpreant = frecibo2.vl_total_Inf
                endif
           else
                let vvalor = frecibo2.vl_recibo
                CALCJURO2(dt_venc2,Data1,vvalor)
                let frecibo2.vl_juros = vvalor      \
                    frecibo2.vl_juros_real = vvalor \
                    frecibo2.Vl_Total_Rec  = (frecibo2.Vl_Recibo +    \
                              {frecibo2.Vl_Juros,0}) - {frecibo2.Vl_Desconto,0}
           endif
           if  {frecibo2.vl_Desc_Inf,0.00}  >  0  or    \
               {frecibo2.vl_desc_inf_juro,0}  >  0
               CRITICA()
          if  vflag = "1"
                   continue
               endif
           endif
        else
           Mens("Tipo Tabela Incorreto!",15,30)
           form clear frecibo2.nr_tab
           form set (cursor) frecibo2.nr_tab
           form display noprompt bell
           continue
        endif

    when $fieldnum = 14  or  $fieldnum = 22    %% CAMPO VL_DESCONTO %%

         CRITICA()
    if   vflag = "1"
              continue
         endif

    when $fieldnum = 15    %% CAMPO AMORTIZA TOTAL %%

         rot_PORCONTA(dt_venc2)
    if   vflag = "1"
              continue
         else
              let  vpreant = frecibo2.vl_total_Inf
         endif

    when $fieldnum = 21    %% DATA CHEQUE  %%
         RotCheque(dataant,vpreant)
         if  vflag = "1"
             continue
         endif

    otherwise  %%%% INCLUSAO DE RECIBO PELOS CAMPOS %%%%
        if  $formexit = "exitkey"
            continue
        endif

%----------
PrepREC:
%---------


        let frecibo2.nr_diasjur_inf = 0 frecibo2.nr_diasjur_Cal = 0
        if  frecibo2.Sg_Loja <> frecibo2.sg_loja_rec
            transaction read
            Compute 1 Contrato wh Contrato.cd_chave_con =\
                frecibo2.Sg_Loja_Rec? eval(1)
       endtransaction
            if  $membercount = 0
                mens("Use F4 p/ Outra Loja!",15,30)
                Continue
            Endif
        Endif

        let vg_descant = 0 vg_juroant = 0

   let vg_ac = frecibo2.tp_fin
   let frecibo2.tp_fin = {"13" wh vg_ac = "14", \
                               " 4" wh vg_ac = " 5", \
                " 3" wh vg_ac = " 2",vg_ac}

        If $FieldNum = 8
           let chvant = $concat(frecibo2.Sg_Loja_Rec," 2",\
                {frecibo2.Tp_Fin," "},frecibo2.Nr_Contrato)
        EndIf

        transaction read
        find all Prestacao wh prestacao.cd_chave_con =\
                $concat(frecibo2.Sg_Loja_Rec," 2",\
                             frecibo2.Tp_Fin,$ToChar(frecibo2.Nr_Contrato,8)) \
                 sorted by nr_prestacao \
           Eval(Let l_DtCanc = {Prestacao.Dt_Cancel,Dt_Zero}) \
               (Let l_DtExclui = {Prestacao.Dt_Exclui,Dt_Zero})       \
          (Let frecibo2.Cd_Cliente       = prestacao.Cd_Cliente) \
               (Let frecibo2.Cd_Representante = prestacao.Cd_Representante) \
               (Let frecibo2.Cd_Divisao       = Prestacao.Cd_Divisao) \
               (Let vg_minuta                 = Prestacao.Nr_Minuta) \
          (Let vtpmin            = prestacao.tp_minuta) \
               (Let vDt                       = Prestacao.Dt_Venda) -> spre2
   endtransaction
        if $setcount < 1 Or \
                (Vg_Anexo = "S" And $IsOdd(fRecibo2.Tp_Fin) = 0)
           let frecibo2.Tp_fin = vg_ac
           let chvant = $concat(frecibo2.Sg_Loja_Rec," 2",\
                             {frecibo2.Tp_Fin," "},frecibo2.Nr_Contrato)
           LIMPA("1")
      mens("CONTRATO INEXISTENTE!",15,30)
           form set (normal  background 8) frecibo2.No_Feriado
      let frecibo2.Nr_Contrato = $substring(chvant,08,8) \
          frecibo2.Sg_Loja_Rec = $substring(chvant,01,3)\
          frecibo2.Tp_Fin      = $substring(chvant,06,2)\
               frecibo2.Tp_Fat      = Vg_TpFat[frecibo2.Tp_Fin]

      form display noprompt
      Continue
        endif

   if $ToDate(l_DtCanc) > 0
      let chvant = $concat(frecibo2.Sg_Loja_Rec," 2",\
                             {frecibo2.Tp_Fin," "},frecibo2.Nr_Contrato)
           LIMPA("1")
      form display noprompt bell
      mens("CONTRATO CANCELADO!",15,30)
           form set (normal  background 8) frecibo2.No_Feriado

      let frecibo2.Nr_Contrato = $substring(chvant,08,8)\
          frecibo2.Sg_Loja_Rec = $substring(chvant,01,3)\
          frecibo2.Tp_Fin      = $substring(chvant,06,2)\
               frecibo2.Tp_Fat      = Vg_TpFat[frecibo2.Tp_Fin]

           form set (cursor) frecibo2.Tp_Fat
      form display noprompt
      Continue
        endif

   if $ToDate(l_DtExclui) > 0
      let chvant = $concat(frecibo2.Sg_Loja_Rec," 2",\
                             {frecibo2.Tp_Fin," "},frecibo2.Nr_Contrato)
           LIMPA("1")
      form display noprompt bell
      mens("CONTRATO  EXCLUIDO!",15,30)
           form set (normal  background 8) frecibo2.No_Feriado

      let frecibo2.Nr_Contrato = $substring(chvant,08,8)\
          frecibo2.Sg_Loja_Rec = $substring(chvant,01,3)\
          frecibo2.Tp_Fin      = $substring(chvant,06,2)\
               frecibo2.Tp_Fat      = Vg_TpFat[frecibo2.Tp_Fin]

           form set (cursor) frecibo2.Tp_Fat
      form display noprompt
      Continue
        endif

   %-let frecibo2.Cd_Cliente       = prestacao.Cd_Cliente \
        %-    frecibo2.Cd_Representante = prestacao.Cd_Representante \
        %-    frecibo2.Cd_Divisao       = Prestacao.Cd_Divisao \
        %-    vg_minuta                 = Prestacao.Nr_Minuta \
   %-    vtpmin            = prestacao.tp_minuta \
        %-    vDt                       = Prestacao.Dt_Venda

        %% LEITURA DO NOME DO CLIENTE
        transaction read
        compute 1 cliente wh cd_chave = \
            $concat(Vg_Regiao,frecibo2.Cd_Cliente)\
                    eval(let vg_nome = $concat(Cd_Cliente,Cliente.No_Cliente))
        endtransaction
        if $membercount = 0
      form display noprompt bell
      mens("ATENCAO!!! Verifique o Cliente.",15,30)
        endif

        if $concat(frecibo2.Sg_Loja_Rec," 2",\
                     frecibo2.Tp_Fin,frecibo2.Nr_Contrato) = {chvant," "} \
                and {Prestant," "} <> frecibo2.Nr_Prestacao

      if frecibo2.Nr_prestacao > {Prestacao.Qt_Prestacao," "}
         Mens("Prestacao Inexistente!",12,20)
         form clear frecibo2.nr_prestacao
         form set (cursor) frecibo2.nr_prestacao
         continue
           elseif frecibo2.Nr_prestacao < Prestant
         Mens("Prestacao Ja Esta Paga!",12,20)
         form clear frecibo2.nr_prestacao
         form set (cursor) frecibo2.nr_prestacao
         continue
           endif
           let var3 = $concat(frecibo2.Sg_Loja_Rec," 2",\
                     frecibo2.Tp_Fin,frecibo2.Nr_Contrato,frecibo2.Nr_Prestacao)
      let prestant = frecibo2.Nr_Prestacao
        else
           Compute 1 spre2 wh Prestacao.vl_saldo_crs <> 0  \
              Eval(Let var3 = Prestacao.Cd_Chave)         \
             (Let prestant = prestacao.nr_prestacao)
           %let var3 = Prestacao.Cd_Chave
      %let prestant = prestacao.nr_prestacao
        endif

   let DataAnt = " " vg_local_desc = "N"
   let Data1   = frecibo2.Dt_Recibo
   let frecibo2.vl_total_inf     = 0.00   \
       frecibo2.vl_desc_inf      = 0.00   \
       frecibo2.vl_desc_inf_juro = 0.00   \
            frecibo2.nr_tab           = "2"    \
            va_tabpre                 = "GP2"
   ROTPRE01(var3,dt_venc2)

   if vflag = "1"
           if vg_clirec="S"
              return
           endif
      LIMPA("1")
        endif

   let frecibo2.vl_total_inf     = 0.00   \
       frecibo2.vl_desc_inf      = 0.00   \
       frecibo2.vl_desc_inf_juro = 0.00
        let chvant = $concat(frecibo2.Sg_Loja_Rec," 2",\
                             frecibo2.Tp_Fin,frecibo2.Nr_Contrato) \
            vPreAnt= 0.00  n1 = 0
        %------------------------------> Promocao Aniversario Paraiba 2005
        If vPc > 0 and fRecibo2.Cd_Divisao = 2 and fRecibo2.Tp_Rec = "PRE" and\
           vDt >= vDV and $Date Between vDI and vDF and  \
           {fRecibo2.Dt_Vencimento, 0} >= $Date and      \
           fRecibo2.Cd_Representante not in (9, 90)
           Let N4 = fRecibo2.Vl_Prestacao
           Let N2 = (N4 * (1-((vPc/1)/100)))
           Let N2 = $Right(N2,2)
           Let N3 = (N4 * (1-((vPc/1)/100)))
           Let vTotal = ($Round(N3 * 10)/10)
           Let fRecibo2.Vl_Desc_Inf  = (fRecibo2.Vl_Prestacao - vTotal)  \
               fRecibo.Cd_Autoriza   = 888888
           Let fRecibo2.Vl_Desconto  = fRecibo2.Vl_Desc_Inf       \
               fRecibo2.Vl_Total_Rec = fRecibo2.Vl_Recibo     +   \
                                       {fRecibo2.Vl_Juros, 0} -   \
                                       {fRecibo2.Vl_Desconto, 0}
        EndIf
  endcase
%set trace off
endwhile

win close wpadrao
endprocedure
