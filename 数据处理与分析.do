cd "/Users/weichenyang/Desktop/学习资料/大二下/econometrics/大作业"
use chip2018_urban_person.dta
keep A03 A13_1 A14 A15_5 A15_7 C02 C03_2 C03_3 C05_1 
drop if A13_1== -99
drop if A13_1== -88
drop if A13_1==1
drop if A13_1==2
drop if A13_1==3
drop if A13_1==4
drop if A13_1==5
drop if A13_1==6
drop if A13_1==9
rename A13_1 edu2
replace edu2=1 if edu2==8
replace edu2=0 if edu2==7
label define repire1 1"大学本科" 0"专科"
label values edu2 repire1
label variable edu2 "=1，个体学历为大学本科；=0，个体学历为专科"
rename A03 male
replace male=0 if male==2
label define repire2 1"male" 0"female"
label values male repire2
label variable male "=1，个体为男性；=0，个体为女性"
drop if C02==-99
drop if C02==-88
gen exp= 2019-C02+1
drop C02
label variable exp"截至2019年末个体的工龄"
*211工程院校（包含985）为重本院校；其余为非重本院校
rename A15_7 edu1
drop if edu1==-99
replace edu1=1 if edu1==1 | edu1==2
replace edu1=0 if edu1==3 | edu1==4 | edu1==5 | edu1==6| edu1==7
label define repire3 1"重本院校" 0"非重本院校" 
label values edu1 repire3
label variable edu1 "=1，个体来自重本院校；=0，个体来自非重本院校"
gen y= C05_1/2432
drop C05_1
label variable y"个体2019年每小时的收入（元/小时）"
gen lny=ln(y)
label variable lny"小时收入的自然对数"
gen sch1= (A14==1)
label variable sch1"=1，个体高中院校为全国或省级重点中学；=0，其他"
gen sch2= (A14==2)
label variable sch2"=1，个体高中院校为地区级重点中学"
gen sch3= (A14==3)
label variable sch3"=1，个体高中院校为县级及其他重点中学"
gen sch4= (A14==4)
replace sch4=1 if A14==5 | A14==6 |A14==7
label variable sch4"=1，个体高中来自非重点中学"
drop A14
drop if A15_5==-99 | A15_5==-88
gen epro1= (A15_5==11 | A15_5==12 | A15_5==31 | A15_5==50)
label variable epro1"=1，来自北京、上海、天津、重庆；=0，其他"
gen epro2= (A15_5==15 | A15_5==45 | A15_5==46 | A15_5==52 | A15_5== 53 | A15_5==54 | A15_5==62 | A15_5==63 | A15_5==64 | A15_5==65)
label variable epro2"=1，来自内蒙古自治区、广西壮族自治区、海南省、贵州、云南、西藏、甘肃、青海、宁夏回族自治区、新疆；=0，其他"
gen epro3= (A15_5==32 | A15_5==33 | A15_5==34 |A15_5== 35 | A15_5== 37 | A15_5==42 | A15_5==43 | A15_5==44 | A15_5==51 | A15_5==61)
label variable epro3"=1，来自江苏、浙江、安徽、福建、山东、湖北、湖南、广东、四川、陕西；=0，其他"
gen epro4= (A15_5==13 | A15_5==14 |A15_5==21 | A15_5==22 | A15_5==23 | A15_5==36 | A15_5== 41)
label variable epro4"=1，来自河北、山西、辽宁、吉林、黑龙江、江西、河南；=0，其他"
drop A15_5
gen uni1= (C03_2==1 |C03_2==2)
label variable uni1"=1，政府机关或事业单位；＝０，其他"
gen uni2= (C03_2==3)
label variable uni2"=1，国有企业；=0，其他"
gen uni3= (C03_2==6)
label variable uni3"=1，中外合资或外商独资企业；=0，其他"
gen uni4= (C03_2==7 | C03_2==4 |C03_2==8 | C03_2==9)
label variable uni4"=1，其他股份制企业、个体或私营企业、土地承包者、其他；=0，其他"
gen uni5= (C03_2==5)
label variable uni5"=1，集体企业；=0，其他"
drop C03_2
drop if C03_3==-99
gen ind1= (C03_3==1)
label variable ind1"=1，从事第一产业工作；=0，其他"
gen ind2= (C03_3==2 | C03_3==3 |C03_3==4 | C03_3==5)
label variable ind2"=1，从事第二产业工作；=0，其他"
gen ind3= (C03_3==6 | C03_3==7| C03_3==8|C03_3==9|C03_3==10|C03_3==11|C03_3==12|C03_3==13|C03_3==14|C03_3==15|C03_3==16|C03_3==17|C03_3==18|C03_3==19|C03_3==20)
label variable ind3"=1，从事第三产业工作；=0，其他"
drop C03_3
save "cleaned_data.dta",replace
* PSM 
*1 edu1 本科院校类型
set seed 20230704
gen tmp=runiform()
sort tmp
global x "male sch1 sch2 sch3 epro1 epro2 epro3"
logout, save(h1) tex replace: psmatch2 edu1 $x, out(lny) kernel comm logit
logout, save(h2) tex replace: pstest $x, both graph
pstest $x, both graph
graph export "h12.png", replace
psgraph
graph export "h2.png", replace
reg lny edu1 male exp uni1 uni2 uni3 uni4 ind1 ind2 , r
est store m1 
reg lny edu1 male exp uni1 uni2 uni3 uni4 ind1 ind2 [pw=_weight], r
est store m2
esttab m1 m2 using ols11.tex, replace star( * 0.10 ** 0.05 *** 0.01) b(%6.3f) t(%6.3f) r2(%9.3f) ar2  mtitles("OLS" "PSM_OLS" )  booktabs page width(\\hsize)
logout, save(h3) tex replace: pstest $x, both
*edu2
logout, save(h4) tex replace: psmatch2 edu2 $x, out(lny) kernel comm logit
reg lny edu2 male exp uni1 uni2 uni3 uni4 ind1 ind2, r
est store m3
reg lny edu2 male exp uni1 uni2 uni3 uni4 ind1 ind2 [pw=_weight], r
est store m4
esttab m3 m4 using ols111.tex, replace star( * 0.10 ** 0.05 *** 0.01) b(%6.3f) t(%6.3f) r2(%9.3f) ar2  mtitles("OLS" "PSM_OLS" )  booktabs page width(\\hsize)

