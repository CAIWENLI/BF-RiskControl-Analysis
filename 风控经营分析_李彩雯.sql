----���ѽ���--ҵ�����
SELECT TO_CHAR(T.APP_DATE,'YYYYMM')�����·�,
       SUM(1)�ܵ���,
       SUM(CASE WHEN T.APPROVED=1 THEN 1 ELSE 0 END)ͨ����,
       SUM(1)-SUM(CASE WHEN T.APPROVED=1 THEN 1 ELSE 0 END)�ܾ���,
       SUM(CASE WHEN T.APPROVED=1 THEN 1 ELSE 0 END)/SUM(1)ͨ����,
       SUM(CASE WHEN T.APPROVED=1 THEN 1 ELSE 0 END)/SUM(CASE WHEN T.INTER_CODE NOT IN 3 THEN 1 ELSE 0 END )���ڲ�����3��ͨ����
FROM   RCAS.V_CU_RISK_CREDIT_SUMMARY T
WHERE  T.APPROVED+T.REJ=1
       AND T.LOAN_TYPE='030'
GROUP BY TO_CHAR(T.APP_DATE,'YYYYMM')
ORDER BY TO_CHAR(T.APP_DATE,'YYYYMM')

----���ѽ���--������
select  tt.*,
        tt1.*
from 
        (select TO_CHAR(T.APP_DATE,'YYYYMM')APP_DATE,
               sum(CASE WHEN T.APPROVED=1 THEN 1 ELSE 0 END)AMOUNT,
               decode(sum(agr_fpd30),0,0,sum(DEF_FPD30)/sum(agr_fpd30))Rfpd30,
               decode(sum(agr_spd30),0,0,sum(DEF_sPD30)/sum(agr_spd30))Rspd30,
               decode(sum(agr_tpd30),0,0,sum(DEF_tPD30)/sum(agr_tpd30))Rtpd30,
               decode(sum(agr_qpd30),0,0,sum(DEF_qPD30)/sum(agr_qpd30))Rqpd30
        from   rcas.v_cu_risk_credit_summary t
        where  t.approved=1
               and t.APP_DATE>=to_date('201501','yyyymm') 
               and t.APP_DATE<=to_date('201608','yyyymm')
        group by TO_CHAR(T.APP_DATE,'YYYYMM')
        order by 1
        )tt
left join
        (select TO_CHAR(T1.APP_DATE,'YYYYMM')APP_DATE,
               sum(1),
               decode (sum(t.agr_fivepd30),0,0,sum(t.DEF_FIVEPD30)/sum(t.agr_fivepd30))Rfivepd30,
               decode (sum(t.agr_sixpd30),0,0,sum(t.DEF_SIXPD30)/sum(t.agr_sixpd30))Rsixpd30,
               decode (sum(t.agr_sevenpd30),0,0,sum(t.DEF_SEVENPD30)/sum(t.agr_sevenpd30))Rsevenpd30
        from   rcas.v_risk_credit_indicators t,
               rcas.v_cu_risk_credit_summary t1
        where  t1.approved=1
               and t.CONTRACT_NO=t1.CONTRACT_NO
               and t1.APP_DATE>=to_date('20150101','yyyymmdd') 
               and t1.APP_DATE<=to_date('20160801','yyyymmdd')
        group by TO_CHAR(T1.APP_DATE,'YYYYMM')
        order by 1
        )tt1
on tt.APP_DATE=tt1.APP_DATE

----�ĺ����
select    
       SUM(d.APPROVED) ͨ����,
       Round((decode(Sum(D.Agr_Fpd30),0,0,sum(D.def_Fpd30)/Sum(D.agr_Fpd30))),4) Rfpd30,
       sum(D.def_Fpd30)||'/'||Sum(D.agr_Fpd30) Fpd30,
       Round((decode(Sum(D.Agr_Spd30),0,0,sum(D.def_Spd30)/Sum(D.agr_Spd30))),4) Rspd30,
       sum(D.def_Spd30)||'/'||Sum(D.agr_Spd30) Spd30,
       Round((decode(Sum(D.Agr_Tpd30),0,0,sum(D.def_Tpd30)/Sum(D.agr_Tpd30))),4) Rtpd30,
       sum(D.def_tpd30)||'/'||Sum(D.agr_Tpd30) Tpd30,
       Round((decode(Sum(D.Agr_Qpd30),0,0,sum(D.def_Qpd30)/Sum(D.agr_Qpd30))),4) Rqpd30,
       sum(D.def_Qpd30)||'/'||Sum(D.agr_Qpd30) Qpd30,
       Round((decode(Sum(afdi.Agr_fivepd30),0,0,sum(afdi.def_fivepd30)/Sum(afdi.agr_fivepd30))),4) Rfivepd30,
       sum(afdi.def_fivepd30)||'/'||Sum(afdi.agr_fivepd30) Fivepd30,
       Round((decode(Sum(afdi.Agr_sixpd30),0,0,sum(afdi.def_sixpd30)/Sum(afdi.agr_sixpd30))),4) RSixpd30,
       sum(afdi.def_sixpd30)||'/'||Sum(afdi.agr_sixpd30) Sixpd30,
       Round((decode(Sum(afdi.Agr_sevenpd30),0,0,sum(afdi.def_sevenpd30)/Sum(afdi.agr_sevenpd30))),4) Rsevenpd30,
       sum(afdi.def_sevenpd30)||'/'||Sum(afdi.agr_sevenpd30) Sevenpd30
from   rcas.v_cu_risk_credit_summary d,
       rcas.v_risk_credit_indicators afdi
where  d.CONTRACT_NO=afdi.CONTRACT_NO
       and d.APP_DATE>=to_date('201501','yyyymm') 
       and d.APP_DATE<=to_date('201608','yyyymm')
group by TO_CHAR(d.APP_DATE,'YYYYMM')
order by 1

----�����������
select tt.*,
       tt1.*
from  (select   app_month,
                to_char(t.state_date,'yyyymm') state_month,
                sum(case when risk_Bucket_Name in ('M0_UNSETTLE','M1','M2','M3') THEN 1 ELSE 0 END) TO_UN,
                sum(decode(risk_Bucket_Name,'M0_SETTLE',greatest(Nvl(n_cur_balance,0)/1000,0))) TO_M0_SETTLE,
                sum(decode(risk_Bucket_Name,'M0_UNSETTLE',greatest(Nvl(n_cur_balance,0)/1000,0))) TO_M0_UNSETTLE,
                sum(decode(risk_bucket_name,'M1',greatest(Nvl(n_cur_balance,0)/1000,0))) TO_M1,
                sum(decode(risk_bucket_name,'M2',greatest(Nvl(n_cur_balance,0)/1000,0))) TO_M2,
                sum(decode(risk_bucket_name,'M3',greatest(Nvl(n_cur_balance,0)/1000,0))) TO_M3,
                sum(decode(risk_bucket_name,'M4',greatest(Nvl(n_cur_balance,0)/1000,0))) TO_M4,
                sum(decode(risk_bucket_name,'M5',greatest(Nvl(n_cur_balance,0)/1000,0))) TO_M5,
                sum(decode(risk_bucket_name,'M6',greatest(Nvl(n_cur_balance,0)/1000,0))) TO_M6,
                sum(decode(risk_bucket_name,'M7',greatest(Nvl(n_cur_balance,0)/1000,0))) TO_M7,
                sum(decode(risk_bucket_name,'M8',greatest(Nvl(n_cur_balance,0)/1000,0))) TO_M8,
                sum(decode(risk_bucket_name,'M9',greatest(Nvl(n_cur_balance,0)/1000,0))) TO_M9,
                sum(decode(risk_bucket_name,'M10',greatest(Nvl(n_cur_balance,0)/1000,0))) TO_M10,
                sum(decode(risk_bucket_name,'M11',greatest(Nvl(n_cur_balance,0)/1000,0))) TO_M11,
                sum(decode(risk_bucket_name,'M12',greatest(Nvl(n_cur_balance,0)/1000,0))) TO_M12
        From
               (Select  t.*,to_char(t2.app_date,'yyyymm') app_month,
                        Nvl((Case When trunc(T1.settle_Date)<=last_day(add_months(trunc(T.state_Date-1),-1)) Then 'M0_SETTLE'
                        when t.cpd<=4 and Risk_Bucket='M1' THEN 'M0_UNSETTLE'
                        Else Risk_Bucket End),'M0_UNSETTLE') Risk_Bucket_Name,
                        T1.Loan_Amount
                from    Ods.Contract_Monthend t,ods.loan_acct T1,rcas.v_cu_risk_credit_summary t2
                Where   t.acct_loan_no=T1.acct_Loan_No 
                        AND t1.contract_no= t2.contract_no
                        And to_number(t.acct_loan_no)>100536
                        And T.state_date=to_date('20160801','yyyymmdd') 
               )t
        group by t.state_date,app_month
        order by 1,2
)tt
left join
        (select  to_char(t2.app_date,'yyyymm') state_month,
                 count(*),
                 sum(t1.loan_amount)/1000,
                 sum(case when add_months(t2.LOAN_DATE,t2.payment_num)<=to_date('20160801','yyyymmdd') then 1 else 0 end)finish_num,
                 sum(t2.PAYMENT_NUM) total_payment
        from     ods.loan_acct t1,
                 rcas.v_cu_risk_credit_summary t2
        where    t1.contract_no= t2.contract_no
                 And to_number(t1.acct_loan_no)>100536
                 and t2.APPROVED='1'
                 and t2.STATUS_EN not in ('100','210','120','240','z','t','d','x')--20160229����δ��Ӹ�����
        group by to_char(t2.app_date,'yyyymm')
        order by 1
)tt1
on tt.app_month=tt1.state_month


----�����ֽ���������
SELECT SUM(1),
       SUM(CASE WHEN T.APPROVED=1 THEN 1 ELSE 0 END)�����ֽ��ͨ����,
       SUM(CASE WHEN T.INTER_CODE NOT IN 3 THEN 1 ELSE 0 END )���ڲ�����3�Ľ����ֽ��,
       SUM(CASE WHEN T.APPROVED=1 THEN 1 ELSE 0 END)/SUM(1)�����ֽ��ͨ����,
       SUM(CASE WHEN T.APPROVED=1 THEN 1 ELSE 0 END)/SUM(CASE WHEN T.INTER_CODE NOT IN 3 THEN 1 ELSE 0 END )���ڲ�����3�Ľ����ֽ��ͨ����,
       decode(sum(agr_fpd30),0,0,sum(DEF_FPD30)/sum(agr_fpd30))Rfpd30,
       decode(sum(agr_spd30),0,0,sum(DEF_sPD30)/sum(agr_spd30))Rspd30,
       decode(sum(agr_tpd30),0,0,sum(DEF_tPD30)/sum(agr_tpd30))Rtpd30,
       decode(sum(agr_qpd30),0,0,sum(DEF_qPD30)/sum(agr_qpd30))Rqpd30,
       decode(sum(agr_qpd30),0,0,sum(case when t.cpd>30 then 1 else 0 end)/sum(agr_qpd30))Rcpd30,
       SUM(T.CREDIT_AMOUNT),
       SUM(T.CREDIT_AMOUNT)/SUM(CASE WHEN T.APPROVED=1 THEN 1 ELSE 0 END)
FROM   RCAS.V_CU_RISK_CREDIT_SUMMARY T
WHERE  T.APPROVED+T.REJ=1
       AND T.SUB_PRODUCT_TYPE='1'

----���Ѵ�-�ֽ��������
select t1.loan_type ��������,
       count(1)����,
       decode(sum(t1.agr_fpd30),0,0,sum(t1.DEF_FPD30)/sum(t1.agr_fpd30))Rfpd30,
       decode(sum(t1.agr_spd30),0,0,sum(t1.DEF_sPD30)/sum(t1.agr_spd30))Rspd30,
       decode(sum(t1.agr_tpd30),0,0,sum(t1.DEF_tPD30)/sum(t1.agr_tpd30))Rtpd30,
       decode(sum(t1.agr_qpd30),0,0,sum(t1.DEF_qPD30)/sum(t1.agr_qpd30))Rqpd30,
       decode (sum(t.agr_fivepd30),0,0,sum(t.DEF_FIVEPD30)/sum(t.agr_fivepd30))Rfivepd30,
       decode (sum(t.agr_sixpd30),0,0,sum(t.DEF_SIXPD30)/sum(t.agr_sixpd30))Rsixpd30,
       decode (sum(t.agr_sevenpd30),0,0,sum(t.DEF_SEVENPD30)/sum(t.agr_sevenpd30))Rsevenpd30
from   rcas.v_risk_credit_indicators t,
       rcas.v_cu_risk_credit_summary t1
where  t1.approved=1
       and t.CONTRACT_NO=t1.CONTRACT_NO
group by t1.loan_type
order by 1

------�����ֽ��ת���ʼ��ֿط���
select tt.city ����,
       tt.rate ת����,
       tt1.loan_type PC_RCPD,
       tt2. rate_1 ɸѡ��
from       
     (select t.city,
              nvl(nums1,0)/nums2 rate
      from
             (select t.city,
                      count(1) nums1
              from    rcas.v_cu_risk_credit_summary t
              where   t.APPROVED+t.rej=1
                      and t.SUB_PRODUCT_TYPE=1
              group by t.city
              )t,
             (select t1.city,
                      count(1) nums2
              from    rcas.v_cu_cash_customer_opt t1
              group by t1.city
              )t1
      where t.CITY(+)=t1.CITY
      )tt
left join
     (select  t.city,
              t.loan_type ,
              decode(sum(agr_qpd30),0,0,sum(case when t.cpd>30 then 1 else 0 end)/sum(agr_qpd30))Rcpd30
      from    rcas.v_cu_risk_credit_summary t
      where   t.approved=1
      group by t.city,t.loan_type
      order by 1,2
      )tt1
on tt.city=tt1.city
left join 
      (select t.city,
              nvl(nums3,0)/nums4 rate_1
       from
             (select t.city,
                     count(1) nums4
              from   rcas.v_cu_risk_credit_summary t
              where  t.APPROVED=1
                     and t.loan_type='030'
              group by t.city
             )t,
             (select t1.city,
                     count(1) nums3
              from   rcas.v_cu_cash_customer_opt t1
              group by t1.city
             )t1
              where t.CITY(+)=t1.CITY
)tt2
on tt1.city=tt2.city
order by tt1.loan_type


----�۳������������
SELECT count(*),
       SUM(CASE WHEN T.APPROVED=1 THEN 1 ELSE 0 END)�۳�ͨ����,
       SUM(CASE WHEN T.INTER_CODE NOT IN 3 THEN 1 ELSE 0 END )���ڲ�����3��
       SUM(CASE WHEN T.APPROVED=1 THEN 1 ELSE 0 END)/count(*)�۳�ͨ����,
       SUM(CASE WHEN T.APPROVED=1 THEN 1 ELSE 0 END)/SUM(CASE WHEN T.INTER_CODE NOT IN 3 THEN 1 ELSE 0 END )���ڲ�����3�ľ۳�ͨ����,
       decode(sum(agr_fpd30),0,0,sum(DEF_FPD30)/sum(agr_fpd30))Rfpd30,
       decode(sum(agr_spd30),0,0,sum(DEF_sPD30)/sum(agr_spd30))Rspd30,
       decode(sum(agr_tpd30),0,0,sum(DEF_tPD30)/sum(agr_tpd30))Rtpd30,
       decode(sum(agr_qpd30),0,0,sum(DEF_qPD30)/sum(agr_qpd30))Rqpd30,
       decode(sum(agr_qpd30),0,0,sum(case when t.cpd>30 then 1 else 0 end)/sum(agr_qpd30))Rcpd30,
       SUM(T.CREDIT_AMOUNT),
       SUM(T.CREDIT_AMOUNT)/SUM(CASE WHEN T.APPROVED=1 THEN 1 ELSE 0 END)
FROM   RCAS.V_CU_RISK_CREDIT_SUMMARY T,
       s1.store_info t1,
       s1.business_contract_cu t2
WHERE  T.APPROVED+T.REJ=1
       AND T.CONTRACT_NO=t2.SERIALNO
       and T2.stores=t1.sno
       and instr (t1.sno,'A')>0

-----�۳Ϻ����ŵ���������ȡ����
���۳ϴ�����ʽ��
select count(*) 
from s1.store_info t
where  t.SNO like '%A'
  and  exists 
          (select 1 from s1.store_info t1 where t1.SNO||'A'=t.SNO)
����Ǫ������ʽ��
select count(*)
from   s1.store_info t,s1.store_info t1
where  T.COMPANY='BQJR'
       AND T1.SNO LIKE'%A'
       AND T.SNO=SUBSTR(T1.SNO,1,11)


----�۳ϡ������ŵ�ռ�����Ʒ���
select P.app_weekend,
       P.Active_Stores �ۼƼ����ŵ�,
       Q.Share_Stores �����ŵ�����,
       Q.Share_Stores/P.Active_Stores �����ŵ�ռ��
from (select ttt.app_weekend,
             sum(count(1)) over(order by ttt.app_weekend) Active_Stores
        from 
             (select  t1.app_weekend
                      ,tt.sno �ŵ����
                      ,tt.sname �ŵ�����
                      ,t1.INPUT_DATE �󶨵�һ�ֲ�Ʒʱ��
                      ,t1.inputuser �����˻�
                      ,t5.inputdate �󶨵�һ������ʱ�� 
              from (select t.*
                    from s1.STORE_INFO  t 
                    WHERE company='JCC'---�ŵ�
                    ) tt
              left join
                   (select a.app_weekend
                          ,a.sno
                          ,a.sname
                          ,trunc(to_date(INPUTDATE,'yyyy-MM-dd HH24:MI:ss')) INPUT_DATE
                          ,a.inputuser 
                    from
                         (select to_char(t3.app_weekend,'yyyymmdd')  app_weekend
                                 ,substr(t4.SNO,1,12) sno
                                 ,t4.SNAME
                                 ,t4.pname--��Ʒϵ������
                                 ,t4.INPUTDATE--��������
                                 ,t4.inputuser --������
                                 ,row_number() over (partition by substr(t4.SNO,1,12) order by t4.inputdate) rn
                          from S1.STORERELATIVEPRODUCT t4,--- �ŵ��Ʒ
                               rcas.v_cu_risk_credit_summary t3,
                               s1.business_contract_cu t2
                          WHERE SNAME IS NOT NULL AND t4.SNO like '%A'
                                AND T3.CONTRACT_NO=t2.SERIALNO
                                and T2.stores=t4.sno
                          order by t4.sno) a     
                    where rn=1  ---�ŵ�󶨵�һ�ֲ�Ʒ��ʱ��(2016��3��30�ţ�
                    ) t1
              on tt.sno=t1.sno
              left join
                  (
                   SELECT sno
                         ,min(trunc(to_date(INPUTDATE,'yyyy-MM-dd HH24:MI:ss'))) inputdate
                   FROM s1.STORERELATIVESALESMAN 
                   WHERE SNO like '%A'    
                   group by sno
                   order by sno
                  ) t5
              on tt.sno=t5.sno
              where t1.inputuser is not null 
              order by �ŵ����
              )ttt
              group by ttt.app_weekend
              order by 1
      )P
left join
     (select to_char(t3.app_weekend,'yyyymmdd') app_weekend_1,
             count(1) Share_Stores
      from   s1.store_info t,s1.store_info t1,
             rcas.v_cu_risk_credit_summary t3,
             s1.business_contract_cu t2
      where  T.COMPANY='BQJR'
             AND T3.CONTRACT_NO=t2.SERIALNO
             and T2.stores=t1.sno
             AND T1.SNO LIKE'%A'
             AND T.SNO=SUBSTR(T1.SNO,1,11)
      group by to_char(t3.app_weekend,'yyyymmdd')
      order by 1
      )Q
on P. app_weekend=Q.app_weekend_1
order by app_weekend



----�����ŵ���ǹ���BQ�ŵ��������������ʶԱ�
select ttt1.app_weekend_1,
       ttt1.sales_1,
       ttt2.sales,
       growth_1,
       growth       
from  (select ttt.sales/lag(ttt.sales)over(order by ttt.app_weekend desc)-1 growth_1,
              ttt.app_weekend,
              ttt.sales
         from
             (select  to_char(tt.app_weekend,'yyyymmdd')app_weekend,
                      count(*)sales
              from    rcas.v_cu_risk_credit_summary tt,
                      s1.store_info tt1,
                      s1.business_contract_cu tt2
              where   tt1.sno=tt2.stores
                      and tt2.SERIALNO=tt.contract_no
                      and tt.POS_CODE in
                               (select t.SNO
                                from   s1.store_info t,s1.store_info t1
                                where  T.COMPANY='BQJR'
                                       AND T1.SNO LIKE'%A'
                                       AND T.SNO=SUBSTR(T1.SNO,1,11)
                                )
                      group by to_char(tt.app_weekend,'yyyymmdd')
                      order by 1
              )ttt
        )ttt2
left join
            (select  tt1.sales_1/lag(tt1.sales_1)over (order by tt1.app_weekend_1 desc)-1 growth,
                     tt1.app_weekend_1,
                     tt1.sales_1
              from   
                    (select to_char(t.app_weekend,'yyyymmdd')app_weekend_1,
                            count(*)sales_1
                      from  rcas.v_cu_risk_credit_summary t,
                            s1.store_info t1,
                            s1.business_contract_cu t2
                      where t1.sno=t2.stores
                            and t2.SERIALNO=t.contract_no
                            and t.POS_CODE not in '%A'
                            and t.approved=1
                            and t.loan_type='020'
                      group by to_char(t.app_weekend,'yyyymmdd')
                      order by 1)tt1
             )ttt1
on ttt2.app_weekend=ttt1.app_weekend_1
where ttt1.app_weekend_1>='20160510'
      and ttt1.app_weekend_1<='20160805'
order by ttt1.app_weekend_1


---�����ŵ꣨BQ��JC�������������ʶԱ�
select ttt1.app_weekend_1,
       ttt1.sales_1 JC�����ŵ�����,
       ttt2.sales BQ�����ŵ�����,
       ttt1.sales_1+ttt2.sales �����ŵ����� ,
       growth_1 JC�����ŵ�����������,
       growth BQ�����ŵ�����������     
from  (select ttt.sales/lag(ttt.sales)over(order by ttt.app_weekend desc)-1 growth,
              ttt.app_weekend,
              ttt.sales
         from
             (select  to_char(tt.app_weekend,'yyyymmdd')app_weekend,
                      count(*)sales
              from    rcas.v_cu_risk_credit_summary tt,
                      s1.store_info tt1,
                      s1.business_contract_cu tt2
              where   tt1.sno=tt2.stores
                      and tt2.SERIALNO=tt.contract_no
                      AND tt.APPROVED=1
                      and tt.POS_CODE in
                               (select t.SNO
                                from   s1.store_info t,s1.store_info t1
                                where  T.COMPANY='BQJR'
                                       AND T1.SNO LIKE'%A'
                                       AND T.SNO=SUBSTR(T1.SNO,1,11)
                                )
                      group by to_char(tt.app_weekend,'yyyymmdd')
                      order by 1
              )ttt
        )ttt2
left join
            (select  tt1.sales_1/lag(tt1.sales_1)over (order by tt1.app_weekend_1 desc)-1 growth_1,
                     tt1.app_weekend_1,
                     tt1.sales_1
              from   
                    (select to_char(t.app_weekend,'yyyymmdd')app_weekend_1,
                            count(*)sales_1
                      from  rcas.v_cu_risk_credit_summary t,
                            s1.store_info t1,
                            s1.business_contract_cu t2
                      where t1.sno=t2.stores
                            and t2.SERIALNO=t.contract_no
                            and t.POS_CODE IN 
                                        (select t.sno
                                          from s1.store_info t
                                          where  t.SNO like '%A'
                                            and  exists 
                                                    (select 1 from s1.store_info t1 where t1.SNO||'A'=t.SNO)
                                        )
                            and t.approved=1
                      group by to_char(t.app_weekend,'yyyymmdd')
                      order by 1)tt1
             )ttt1
on ttt2.app_weekend=ttt1.app_weekend_1
where ttt1.app_weekend_1>='20160510'
      and ttt1.app_weekend_1<='20160805'
order by ttt1.app_weekend_1

---�۳ϡ���Ǫ��������ط���
select *
from (select  count(*)sales,
              decode(sum(tt.agr_fpd30),0,0,sum(tt.DEF_FPD30)/sum(tt.agr_fpd30))Rfpd30,
              decode(sum(tt.agr_qpd30),0,0,sum(case when tt.cpd>30 then 1 else 0 end)/sum(tt.agr_qpd30))Rcpd30
      from    rcas.v_cu_risk_credit_summary tt,
              s1.store_info tt1
      where   tt1.sno=tt.pos_code
              AND tt.APPROVED=1
              and tt.POS_CODE in
                       (select t.SNO
                        from   s1.store_info t,s1.store_info t1
                        where  T.COMPANY='BQJR'
                               AND T1.SNO LIKE'%A'
                               AND T.SNO=SUBSTR(T1.SNO,1,11)
                        )
       )
     union 
     (select  count(*)sales_1,
              decode(sum(tt.agr_fpd30),0,0,sum(tt.DEF_FPD30)/sum(tt.agr_fpd30))Rfpd30_1,
              decode(sum(tt.agr_qpd30),0,0,sum(case when tt.cpd>30 then 1 else 0 end)/sum(tt.agr_qpd30))Rcpd30_1
      from    rcas.v_cu_risk_credit_summary tt,
              s1.store_info tt1
      where   tt1.sno=tt.pos_code
              AND tt.APPROVED=1
              AND tt1.SNO not in '%A'
              and tt.POS_CODE not in
                       (select t.SNO
                        from   s1.store_info t,
                               s1.store_info t1
                        where  T.COMPANY='BQJR'
                               AND T1.SNO LIKE'%A'
                               AND T.SNO=SUBSTR(T1.SNO,1,11)
                       )
     )
     union
     (select  count(*)sales_2,
              decode(sum(tt.agr_fpd30),0,0,sum(tt.DEF_FPD30)/sum(tt.agr_fpd30))Rfpd30_2,
              decode(sum(tt.agr_qpd30),0,0,sum(case when tt.cpd>30 then 1 else 0 end)/sum(tt.agr_qpd30))Rcpd30_2
      from    rcas.v_cu_risk_credit_summary tt,
              s1.store_info tt1
      where   tt1.sno=tt.pos_code
              AND tt.APPROVED=1
              and tt.SURE_TYPE='JCC'
              and tt.POS_CODE not in
                       (select t.sno
                            from s1.store_info t
                            where  t.SNO like '%A'
                              and  exists 
                                      (select 1 from s1.store_info t1 where t1.SNO||'A'=t.SNO)
                        )
     )
     union
       (select count(*)sales_3,
               decode(sum(tt.agr_fpd30),0,0,sum(tt.DEF_FPD30)/sum(tt.agr_fpd30))Rfpd30_3,
               decode(sum(tt.agr_qpd30),0,0,sum(case when tt.cpd>30 then 1 else 0 end)/sum(tt.agr_qpd30))Rcpd30_3
        from   rcas.v_cu_risk_credit_summary tt,
               s1.store_info tt1
        where  tt1.sno=tt.pos_code
               and tt.SURE_TYPE='JCC'
               and tt.approved=1
               and tt.POS_CODE IN 
                          (select t.sno
                            from s1.store_info t
                            where  t.SNO like '%A'
                              and  exists 
                                      (select 1 from s1.store_info t1 where t1.SNO||'A'=t.SNO)
                           )
        )

---�������ſ�
SELECT t1.region,
       count(1),
       SUM(CASE WHEN T.APPROVED=1 THEN 1 ELSE 0 END)ͨ����,
       SUM(CASE WHEN T.INTER_CODE NOT IN 3 THEN 1 ELSE 0 END )���ڲ�����3��
       SUM(CASE WHEN T.APPROVED=1 THEN 1 ELSE 0 END)/SUM(1)ͨ����,
       SUM(CASE WHEN T.APPROVED=1 THEN 1 ELSE 0 END)/SUM(CASE WHEN T.INTER_CODE NOT IN 3 THEN 1 ELSE 0 END )���ڲ�����3�Ľ����ֽ��ͨ����,
       decode(sum(agr_qpd30),0,0,sum(case when t.cpd>90 then 1 else 0 end)/sum(agr_qpd30))Rcpd90
FROM   RCAS.V_CU_RISK_CREDIT_SUMMARY T,
       AF.REGION_1 t1
WHERE  T.APPROVED+T.REJ=1
       and t.city=t1.city
group by t1.region
order by 1


----��Ʒ�׶λ���
select t.PRODUCTCATEGORYNAME,
       t.SUB_PRODUCT_TYPE,
       count(*)
from rcas.v_cu_risk_credit_summary t
group by  t.PRODUCTCATEGORYNAME,t.SUB_PRODUCT_TYPE
order by 1,2
