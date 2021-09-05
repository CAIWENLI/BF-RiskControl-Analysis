----消费金融--业务情况
SELECT TO_CHAR(T.APP_DATE,'YYYYMM')申请月份,
       SUM(1)总单量,
       SUM(CASE WHEN T.APPROVED=1 THEN 1 ELSE 0 END)通过量,
       SUM(1)-SUM(CASE WHEN T.APPROVED=1 THEN 1 ELSE 0 END)拒绝量,
       SUM(CASE WHEN T.APPROVED=1 THEN 1 ELSE 0 END)/SUM(1)通过率,
       SUM(CASE WHEN T.APPROVED=1 THEN 1 ELSE 0 END)/SUM(CASE WHEN T.INTER_CODE NOT IN 3 THEN 1 ELSE 0 END )除内部代码3的通过率
FROM   RCAS.V_CU_RISK_CREDIT_SUMMARY T
WHERE  T.APPROVED+T.REJ=1
       AND T.LOAN_TYPE='030'
GROUP BY TO_CHAR(T.APP_DATE,'YYYYMM')
ORDER BY TO_CHAR(T.APP_DATE,'YYYYMM')

----消费金融--风控情况
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

----改后代码
select    
       SUM(d.APPROVED) 通过量,
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

----不良贷款情况
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
                 and t2.STATUS_EN not in ('100','210','120','240','z','t','d','x')--20160229数据未添加该条件
        group by to_char(t2.app_date,'yyyymm')
        order by 1
)tt1
on tt.app_month=tt1.state_month


----交叉现金贷总体情况
SELECT SUM(1),
       SUM(CASE WHEN T.APPROVED=1 THEN 1 ELSE 0 END)交叉现金贷通过量,
       SUM(CASE WHEN T.INTER_CODE NOT IN 3 THEN 1 ELSE 0 END )除内部代码3的交叉现金贷,
       SUM(CASE WHEN T.APPROVED=1 THEN 1 ELSE 0 END)/SUM(1)交叉现金贷通过率,
       SUM(CASE WHEN T.APPROVED=1 THEN 1 ELSE 0 END)/SUM(CASE WHEN T.INTER_CODE NOT IN 3 THEN 1 ELSE 0 END )除内部代码3的交叉现金贷通过率,
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

----消费贷-现金贷风控情况
select t1.loan_type 贷款类型,
       count(1)单量,
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

------交叉现金贷转化率及分控分析
select tt.city 城市,
       tt.rate 转化率,
       tt1.loan_type PC_RCPD,
       tt2. rate_1 筛选率
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


----聚诚销量整体情况
SELECT count(*),
       SUM(CASE WHEN T.APPROVED=1 THEN 1 ELSE 0 END)聚诚通过量,
       SUM(CASE WHEN T.INTER_CODE NOT IN 3 THEN 1 ELSE 0 END )除内部代码3，
       SUM(CASE WHEN T.APPROVED=1 THEN 1 ELSE 0 END)/count(*)聚诚通过率,
       SUM(CASE WHEN T.APPROVED=1 THEN 1 ELSE 0 END)/SUM(CASE WHEN T.INTER_CODE NOT IN 3 THEN 1 ELSE 0 END )除内部代码3的聚诚通过率,
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

-----聚诚合作门店量（两种取法）
（聚诚代码形式）
select count(*) 
from s1.store_info t
where  t.SNO like '%A'
  and  exists 
          (select 1 from s1.store_info t1 where t1.SNO||'A'=t.SNO)
（佰仟代码形式）
select count(*)
from   s1.store_info t,s1.store_info t1
where  T.COMPANY='BQJR'
       AND T1.SNO LIKE'%A'
       AND T.SNO=SUBSTR(T1.SNO,1,11)


----聚诚、共存门店占比趋势分析
select P.app_weekend,
       P.Active_Stores 累计激活门店,
       Q.Share_Stores 共存门店数量,
       Q.Share_Stores/P.Active_Stores 共存门店占比
from (select ttt.app_weekend,
             sum(count(1)) over(order by ttt.app_weekend) Active_Stores
        from 
             (select  t1.app_weekend
                      ,tt.sno 门店代码
                      ,tt.sname 门店名称
                      ,t1.INPUT_DATE 绑定第一种产品时间
                      ,t1.inputuser 操作账户
                      ,t5.inputdate 绑定第一个销售时间 
              from (select t.*
                    from s1.STORE_INFO  t 
                    WHERE company='JCC'---门店
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
                                 ,t4.pname--产品系列名称
                                 ,t4.INPUTDATE--登入日期
                                 ,t4.inputuser --登入人
                                 ,row_number() over (partition by substr(t4.SNO,1,12) order by t4.inputdate) rn
                          from S1.STORERELATIVEPRODUCT t4,--- 门店产品
                               rcas.v_cu_risk_credit_summary t3,
                               s1.business_contract_cu t2
                          WHERE SNAME IS NOT NULL AND t4.SNO like '%A'
                                AND T3.CONTRACT_NO=t2.SERIALNO
                                and T2.stores=t4.sno
                          order by t4.sno) a     
                    where rn=1  ---门店绑定第一种产品的时间(2016年3月30号）
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
              order by 门店代码
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



----共存门店与非共存BQ门店销量及其增长率对比
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


---共存门店（BQ、JC）销量及增长率对比
select ttt1.app_weekend_1,
       ttt1.sales_1 JC共存门店销量,
       ttt2.sales BQ共存门店销量,
       ttt1.sales_1+ttt2.sales 共存门店销量 ,
       growth_1 JC共存门店销售增长率,
       growth BQ共存门店销售增长率     
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

---聚诚、佰仟销量及风控分析
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

---三个区概况
SELECT t1.region,
       count(1),
       SUM(CASE WHEN T.APPROVED=1 THEN 1 ELSE 0 END)通过量,
       SUM(CASE WHEN T.INTER_CODE NOT IN 3 THEN 1 ELSE 0 END )除内部代码3，
       SUM(CASE WHEN T.APPROVED=1 THEN 1 ELSE 0 END)/SUM(1)通过率,
       SUM(CASE WHEN T.APPROVED=1 THEN 1 ELSE 0 END)/SUM(CASE WHEN T.INTER_CODE NOT IN 3 THEN 1 ELSE 0 END )除内部代码3的交叉现金贷通过率,
       decode(sum(agr_qpd30),0,0,sum(case when t.cpd>90 then 1 else 0 end)/sum(agr_qpd30))Rcpd90
FROM   RCAS.V_CU_RISK_CREDIT_SUMMARY T,
       AF.REGION_1 t1
WHERE  T.APPROVED+T.REJ=1
       and t.city=t1.city
group by t1.region
order by 1


----产品阶段汇总
select t.PRODUCTCATEGORYNAME,
       t.SUB_PRODUCT_TYPE,
       count(*)
from rcas.v_cu_risk_credit_summary t
group by  t.PRODUCTCATEGORYNAME,t.SUB_PRODUCT_TYPE
order by 1,2
