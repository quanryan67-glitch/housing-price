cd "/Users/quanrongxi/Desktop/科研/博四/房价断点识别/任务分解/任务0523"

use 年月结果汇总_m1_所有城市,clear

merge 1:1 city using city_classification
keep if _merge == 3
drop _merge

drop break4_ym break5_ym number

save 城市断点信息_0523,replace
 
 
 

************************
use 城市价格指数_x12调整后,clear

**城市经济信息匹配
merge m:1 city using 城市断点信息_0523
keep if _merge == 3
drop _merge

merge m:1 city using 供需比与PRR_260602
keep if _merge == 3
drop _merge

merge m:1 city using 城市经济变量_2020_284城_260602
keep if _merge == 3
drop _merge

rename month ym

egen city_id = group(city)
order city_id

xtset city_id ym

bys city_id : gen gr_pri = (price_index_x12 - l.price_index_x12) / l.price_index_x12


*月环比增速均值(上升期结束后)
bys city_id: egen avg_gr_post = mean(gr_pri) if ym > T0
bys city_id: fillmissing avg_gr_post
label var avg_gr_post "上升结束后的平均月环比增速"

*月环比增速均值(整个上升期，从上升开始到上升结束)
bys city_id: egen avg_gr_before = mean(gr_pri) if ym > T0_start & ym <= T0
bys city_id: fillmissing avg_gr_before
label var avg_gr_before "上升期的平均月环比增速"

*固定窗口期变化幅度（上升期结束后）
bys city_id: gen magni_post6 = (price_index_x12[_n+6] - price_index_x12[_n])/  price_index_x12[_n] if ym == T0
bys city_id: gen magni_post12 = (price_index_x12[_n+12] - price_index_x12[_n])/  price_index_x12[_n] if ym == T0
bys city_id: gen magni_post18 = (price_index_x12[_n+18] - price_index_x12[_n])/  price_index_x12[_n] if ym == T0

bys city_id: fillmissing magni_post6 magni_post12 magni_post18
label var magni_post6  "上升结束后的6个月的总变化幅度"
label var magni_post12 "上升结束后的12个月的总变化幅度"
label var magni_post18 "上升结束后的18个月的总变化幅度"


save sample_ym,replace

**# PRR/SDR与下跌幅度
use sample_ym,clear
* ==============================================================================
* 一：研究问题
* 对于存在上涨结束点T0的城市，PRR更高的城市在T0之后是否跌得更惨？
* 识别思路：在T0前后窗口内，比较高/低PRR城市的动态房价变化。
* 回归控制城市固定效应和日历年月固定效应，标准误聚类到城市层面。
* ==============================================================================

keep if T0 != .

local prr prr3
xtile x_bp = `prr', n(2)
gen high_prr = x_bp - 1
drop x_bp
label define high_prr_lb 0 "低PRR" 1 "高PRR"
label values high_prr high_prr_lb
label var high_prr "PRR高于样本中位数"

egen prr_z = std(`prr')
label var prr_z "标准化PRR"

local sdr sdr_unit3
xtile x_bp = `sdr', n(2)
gen high_sdr = x_bp - 1
drop x_bp
label define high_sdr_lb 0 "低SDR" 1 "高SDR"
label values high_sdr high_sdr_lb
label var high_sdr "SDR高于样本中位数"

egen sdr_z = std(`sdr')
label var sdr_z "标准化SDR"

gen relative_time = ym - T0
label var relative_time "相对时间（月，T0=0）"

gen ln_price = ln(price_index_x12)
bys city_id (ym): gen dln_price = 100 * (ln_price - ln_price[_n-1])
label var dln_price "房价指数月度对数涨幅（%）"

tempfile prr_month_panel
save `prr_month_panel', replace

// gen event_window = inrange(relative_time, -24, 24)
// keep if event_window

bys city_id: egen ln_price_T0 = max(cond(relative_time == 0, ln_price, .))
gen cum_ln_price_T0 = 100 * (ln_price - ln_price_T0)
label var cum_ln_price_T0 "相对T0的累计房价变化（对数百分点）"

replace relative_time = 24 if relative_time > 24
replace relative_time = -24 if relative_time < -24

gen post = relative_time >= 0
label var post "T0及之后"

* 因子变量不能方便地把负数相对时间设为基准，因此平移到1-49。
* rel_shift=24 对应 relative_time=-1，作为事件研究基准期。
gen rel_shift = relative_time + 25
label var rel_shift "平移后的相对时间（24对应T0前1月）"


reghdfe dln_price i.post, absorb(city_id ym) vce(cluster city_id)


reghdfe dln_price ib24.rel_shift, ///
    absorb(city_id ym) vce(cluster city_id)
est store es

coefplot es, ///
    keep(*.rel_shift) ///
    vertical omitted baselevels ///
    recast(connected) ciopts(recast(rcap) lcolor(maroon%45)) ///
    mcolor(maroon) lcolor(maroon) msize(small) ///
    yline(0, lpattern(dash) lcolor(gs10)) ///
    xline(24, lpattern(dot) lcolor(gs8)) ///
	xlabel(, labsize(vsmall)) ///
    coeflabels( ///
        1.rel_shift  = "-24" ///
        2.rel_shift  = "-23" ///
        3.rel_shift = "-22" ///
        4.rel_shift  = "-21" ///
        5.rel_shift  = "-20" ///
        6.rel_shift  = "-19" ///
        7.rel_shift  = "-18" ///
        8.rel_shift  = "-17" ///
        9.rel_shift = "-16" ///
        10.rel_shift = "-15" ///
        11.rel_shift = "-14" ///
        12.rel_shift = "-13" ///
        13.rel_shift = "-12" ///
        14.rel_shift = "-11" ///
        15.rel_shift = "-10" ///
        16.rel_shift = "-9" ///
        17.rel_shift = "-8" ///
        18.rel_shift = "-7" ///
        19.rel_shift = "-6" ///
        20.rel_shift = "-5" ///
        21.rel_shift = "-4" ///
        22.rel_shift = "-3" ///
        23.rel_shift = "-2" ///
        24.rel_shift = "-1" ///
        25.rel_shift = "0" ///
        26.rel_shift = "1" ///
        27.rel_shift = "2" ///
        28.rel_shift = "3" ///
        29.rel_shift = "4" ///
        30.rel_shift = "5" ///
        31.rel_shift = "6" ///
        32.rel_shift = "7" ///
        33.rel_shift = "8" ///
        34.rel_shift = "9" ///
        35.rel_shift = "10" ///
        36.rel_shift = "11" ///
        37.rel_shift = "12" ///
        38.rel_shift = "13" ///
        39.rel_shift = "14" ///
        40.rel_shift = "15" ///
        41.rel_shift = "16" ///
        42.rel_shift = "17" ///
        43.rel_shift = "18" ///
        44.rel_shift = "19" ///
        45.rel_shift = "20" ///
        46.rel_shift = "21" ///
        47.rel_shift = "22" ///
        48.rel_shift = "23" ///
        49.rel_shift = "24") ///
    xtitle("相对T0的月份") ///
    ytitle("房价环比") ///
    title("事件研究：城市上升结束前后房价增速变化") ///
    subtitle("基准期为T0前1月；控制城市固定效应和日历年月固定效应") ///
    legend(off) ///
    scheme(s1color) graphregion(color(white)) plotregion(color(white))

* ==============================================================================
* 二：描述性平行趋势图 - 高/低PRR城市的累计房价路径
* ==============================================================================

preserve
collapse (mean) mean_cum = cum_ln_price_T0 ///
         (semean) se_cum = cum_ln_price_T0, by(high_prr relative_time)
gen ci_l = mean_cum - 1.96 * se_cum
gen ci_u = mean_cum + 1.96 * se_cum

twoway ///
    (rarea ci_l ci_u relative_time if high_prr == 0, sort color(navy%12) lcolor(none)) ///
    (line mean_cum relative_time if high_prr == 0, sort lcolor(navy) lwidth(medthick)) ///
    (rarea ci_l ci_u relative_time if high_prr == 1, sort color(maroon%12) lcolor(none)) ///
    (line mean_cum relative_time if high_prr == 1, sort lcolor(maroon) lwidth(medthick)), ///
    xline(0, lpattern(dash) lcolor(gs8)) ///
    yline(0, lpattern(dash) lcolor(gs10)) ///
    xtitle("相对T0的月份") ///
    ytitle("相对T0的累计房价变化（对数百分点）") ///
    title("高/低PRR城市在上涨结束点前后的房价路径") ///
    subtitle("样本：有上涨结束点T0且PRR非缺失的城市；阴影为95%置信区间") ///
    legend(order(2 "低PRR" 4 "高PRR") rows(1) position(6)) ///
    scheme(s1color) graphregion(color(white)) plotregion(color(white))

graph export "平行趋势_高低PRR_累计房价路径.png", replace
restore

* ==============================================================================
* 三：事件研究回归 - 高PRR相对低PRR的动态月度涨幅差异
* ==============================================================================

reghdfe dln_price ib24.rel_shift##i.high_prr, ///
    absorb(city_id ym) vce(cluster city_id)
est store es_highprr

coefplot es_highprr, ///
    keep(*.rel_shift#1.high_prr) ///
    vertical omitted baselevels ///
    recast(connected) ciopts(recast(rcap) lcolor(maroon%45)) ///
    mcolor(maroon) lcolor(maroon) msize(small) ///
    yline(0, lpattern(dash) lcolor(gs10)) ///
    xline(24, lpattern(dot) lcolor(gs8)) ///
	xlabel(, labsize(vsmall)) ///
    coeflabels( ///
        1.rel_shift#1.high_prr  = "-24" ///
        2.rel_shift#1.high_prr  = "-23" ///
        3.rel_shift#1.high_prr  = "-22" ///
        4.rel_shift#1.high_prr  = "-21" ///
        5.rel_shift#1.high_prr  = "-20" ///
        6.rel_shift#1.high_prr  = "-19" ///
        7.rel_shift#1.high_prr  = "-18" ///
        8.rel_shift#1.high_prr  = "-17" ///
        9.rel_shift#1.high_prr  = "-16" ///
        10.rel_shift#1.high_prr = "-15" ///
        11.rel_shift#1.high_prr = "-14" ///
        12.rel_shift#1.high_prr = "-13" ///
        13.rel_shift#1.high_prr = "-12" ///
        14.rel_shift#1.high_prr = "-11" ///
        15.rel_shift#1.high_prr = "-10" ///
        16.rel_shift#1.high_prr = "-9" ///
        17.rel_shift#1.high_prr = "-8" ///
        18.rel_shift#1.high_prr = "-7" ///
        19.rel_shift#1.high_prr = "-6" ///
        20.rel_shift#1.high_prr = "-5" ///
        21.rel_shift#1.high_prr = "-4" ///
        22.rel_shift#1.high_prr = "-3" ///
        23.rel_shift#1.high_prr = "-2" ///
        24.rel_shift#1.high_prr = "-1" ///
        25.rel_shift#1.high_prr = "0" ///
        26.rel_shift#1.high_prr = "1" ///
        27.rel_shift#1.high_prr = "2" ///
        28.rel_shift#1.high_prr = "3" ///
        29.rel_shift#1.high_prr = "4" ///
        30.rel_shift#1.high_prr = "5" ///
        31.rel_shift#1.high_prr = "6" ///
        32.rel_shift#1.high_prr = "7" ///
        33.rel_shift#1.high_prr = "8" ///
        34.rel_shift#1.high_prr = "9" ///
        35.rel_shift#1.high_prr = "10" ///
        36.rel_shift#1.high_prr = "11" ///
        37.rel_shift#1.high_prr = "12" ///
        38.rel_shift#1.high_prr = "13" ///
        39.rel_shift#1.high_prr = "14" ///
        40.rel_shift#1.high_prr = "15" ///
        41.rel_shift#1.high_prr = "16" ///
        42.rel_shift#1.high_prr = "17" ///
        43.rel_shift#1.high_prr = "18" ///
        44.rel_shift#1.high_prr = "19" ///
        45.rel_shift#1.high_prr = "20" ///
        46.rel_shift#1.high_prr = "21" ///
        47.rel_shift#1.high_prr = "22" ///
        48.rel_shift#1.high_prr = "23" ///
        49.rel_shift#1.high_prr = "24") ///
    xtitle("相对T0的月份") ///
    ytitle("高PRR相对低PRR的月度涨幅差异（百分点）") ///
    title("事件研究：高PRR城市是否在T0后跌得更惨") ///
    subtitle("基准期为T0前1月；控制城市固定效应和日历年月固定效应") ///
    legend(off) ///
    scheme(s1color) graphregion(color(white)) plotregion(color(white))

graph export "事件研究_高PRR动态效应.png", replace 

di "========================================================================"
di "平行趋势检验：H0 = T0前24月至T0前2月的高PRR动态系数共同为0"
di "========================================================================"
matrix b = e(b)
local coefnames : colnames b
local pre_terms
forvalues s = 1/23 {
    local term "`s'.rel_shift#1.high_prr"
    if strpos(" `coefnames' ", " `term' ") local pre_terms `pre_terms' `term'
}
if "`pre_terms'" != "" test `pre_terms'

di "========================================================================"
di "事件后效应：T0至T0后24月高PRR动态系数共同为0"
di "========================================================================"
local post_terms
forvalues s = 25/49 {
    local term "`s'.rel_shift#1.high_prr"
    if strpos(" `coefnames' ", " `term' ") local post_terms `post_terms' `term'
}
if "`post_terms'" != "" test `post_terms'

* ==============================================================================
* 四：简化DID与连续PRR版本
* ==============================================================================

reghdfe dln_price i.post##i.high_prr, absorb(city_id ym) vce(cluster city_id)
est store did_highprr

reghdfe dln_price i.post##c.prr_z, absorb(city_id ym) vce(cluster city_id)
est store did_prrz

di "========================================================================"
di "解释：did_highprr 中 1.post#1.high_prr < 0 表示高PRR城市T0后月度涨幅更低。"
di "解释：did_prrz 中 1.post#c.prr_z < 0 表示PRR每高1个标准差，T0后月度涨幅更低。"
di "========================================================================"

* ==============================================================================
* 五：季度层面事件研究 - 降低月度噪声
* ==============================================================================

use `prr_month_panel', clear

gen q = qofd(dofm(ym))
format q %tq
gen T0_q = qofd(dofm(T0))
format T0_q %tq

collapse (mean) ln_price_q = ln_price ///
         (firstnm) high_prr prr_z T0_q, by(city_id q)

xtset city_id q
gen dln_price_q = 100 * (ln_price_q - L.ln_price_q)
label var dln_price_q "房价指数季度对数涨幅（%）"

gen relative_q = q - T0_q
label var relative_q "相对时间（季度，T0所在季度=0）"

// keep if inrange(relative_q, -8, 8)


bys city_id: egen ln_price_T0_q = max(cond(relative_q == 0, ln_price_q, .))
gen cum_ln_price_T0_q = 100 * (ln_price_q - ln_price_T0_q)
label var cum_ln_price_T0_q "相对T0所在季度的累计房价变化（对数百分点）"

replace relative_q = 8 if relative_q > 8
replace relative_q = -8 if relative_q < -8

gen post_q = relative_q >= 0
label var post_q "T0所在季度及之后"

* rel_q_shift=8 对应 relative_q=-1，作为季度事件研究基准期。
gen rel_q_shift = relative_q + 9
label var rel_q_shift "平移后的相对季度（8对应T0前1季度）"

* 描述性季度路径图
preserve
collapse (mean) mean_cum_q = cum_ln_price_T0_q ///
         (semean) se_cum_q = cum_ln_price_T0_q, by(high_prr relative_q)
gen ci_l_q = mean_cum_q - 1.96 * se_cum_q
gen ci_u_q = mean_cum_q + 1.96 * se_cum_q

twoway ///
    (rarea ci_l_q ci_u_q relative_q if high_prr == 0, sort color(navy%12) lcolor(none)) ///
    (line mean_cum_q relative_q if high_prr == 0, sort lcolor(navy) lwidth(medthick)) ///
    (rarea ci_l_q ci_u_q relative_q if high_prr == 1, sort color(maroon%12) lcolor(none)) ///
    (line mean_cum_q relative_q if high_prr == 1, sort lcolor(maroon) lwidth(medthick)), ///
    xline(0, lpattern(dash) lcolor(gs8)) ///
    yline(0, lpattern(dash) lcolor(gs10)) ///
    xtitle("相对T0所在季度的季度数") ///
    ytitle("相对T0所在季度的累计房价变化（对数百分点）") ///
    title("季度层面：高/低PRR城市在上涨结束点前后的房价路径") ///
    subtitle("季度房价为季度内月度房价指数均值；阴影为95%置信区间") ///
    legend(order(2 "低PRR" 4 "高PRR") rows(1) position(6)) ///
    scheme(s1color) graphregion(color(white)) plotregion(color(white))

graph export "季度平行趋势_高低PRR_累计房价路径.png", replace 
restore

* 季度事件研究回归与coefplot
reghdfe dln_price_q ib8.rel_q_shift##i.high_prr, ///
    absorb(city_id q) vce(cluster city_id)
est store es_highprr_q

coefplot es_highprr_q, ///
    keep(*.rel_q_shift#1.high_prr) ///
    vertical omitted baselevels ///
    recast(connected) ciopts(recast(rcap) lcolor(maroon%45)) ///
    mcolor(maroon) lcolor(maroon) msize(small) ///
    yline(0, lpattern(dash) lcolor(gs10)) ///
    xline(8, lpattern(dot) lcolor(gs8)) ///
    coeflabels( ///
        1.rel_q_shift#1.high_prr  = "-8" ///
        2.rel_q_shift#1.high_prr  = "-7" ///
        3.rel_q_shift#1.high_prr  = "-6" ///
        4.rel_q_shift#1.high_prr  = "-5" ///
        5.rel_q_shift#1.high_prr  = "-4" ///
        6.rel_q_shift#1.high_prr  = "-3" ///
        7.rel_q_shift#1.high_prr  = "-2" ///
        8.rel_q_shift#1.high_prr = "-1" ///
        9.rel_q_shift#1.high_prr  = "0" ///
        10.rel_q_shift#1.high_prr = "1" ///
        11.rel_q_shift#1.high_prr = "2" ///
        12.rel_q_shift#1.high_prr = "3" ///
        13.rel_q_shift#1.high_prr = "4" ///
        14.rel_q_shift#1.high_prr = "5" ///
        15.rel_q_shift#1.high_prr = "6" ///
        16.rel_q_shift#1.high_prr = "7" ///
        17.rel_q_shift#1.high_prr = "8") ///
    xtitle("相对T0所在季度的季度数") ///
    ytitle("高PRR相对低PRR的季度涨幅差异（百分点）") ///
    title("季度事件研究：高PRR城市是否在T0后跌得更惨") ///
    subtitle("基准期为T0前1季度；控制城市固定效应和日历季度固定效应") ///
    legend(off) ///
    scheme(s1color) graphregion(color(white)) plotregion(color(white))

graph export "季度事件研究_高PRR动态效应.png", replace 


di "========================================================================"
di "季度平行趋势检验：H0 = T0前8季度至T0前2季度的高PRR动态系数共同为0"
di "========================================================================"
matrix b = e(b)
local coefnames_q : colnames b
local pre_terms_q
forvalues s = 1/7 {
    local term "`s'.rel_q_shift#1.high_prr"
    if strpos(" `coefnames_q' ", " `term' ") local pre_terms_q `pre_terms_q' `term'
}
if "`pre_terms_q'" != "" test `pre_terms_q'

di "========================================================================"
di "季度事件后效应：T0所在季度至T0后8季度高PRR动态系数共同为0"
di "========================================================================"
local post_terms_q
forvalues s = 9/17 {
    local term "`s'.rel_q_shift#1.high_prr"
    if strpos(" `coefnames_q' ", " `term' ") local post_terms_q `post_terms_q' `term'
}
if "`post_terms_q'" != "" test `post_terms_q'

reghdfe dln_price_q i.post_q##i.high_prr, absorb(city_id q) vce(cluster city_id) keepsin
est store did_highprr_q

reghdfe dln_price_q i.post_q##c.prr_z, absorb(city_id q) vce(cluster city_id) keepsin
est store did_prrz_q

di "========================================================================"
di "季度解释：did_highprr_q 中 1.post_q#1.high_prr < 0 表示高PRR城市T0后季度涨幅更低。"
di "季度解释：did_prrz_q 中 1.post_q#c.prr_z < 0 表示PRR每高1个标准差，T0后季度涨幅更低。"
di "========================================================================"


* ==============================================================================
* 六：SDR版本 - 月度层面事件研究
* ==============================================================================

use `prr_month_panel', clear
keep if sdratio2_unit != .

// gen event_window_sdr = inrange(relative_time, -24, 24)
// keep if event_window_sdr

bys city_id: egen ln_price_T0_sdr = max(cond(relative_time == 0, ln_price, .))
gen cum_ln_price_T0_sdr = 100 * (ln_price - ln_price_T0_sdr)
label var cum_ln_price_T0_sdr "相对T0的累计房价变化（对数百分点）"

replace relative_time = 24 if relative_time > 24
replace relative_time = -24 if relative_time < -24

gen post_sdr = relative_time >= 0
label var post_sdr "T0及之后"

gen rel_shift_sdr = relative_time + 25
label var rel_shift_sdr "平移后的相对时间（24对应T0前1月）"

preserve
collapse (mean) mean_cum_sdr = cum_ln_price_T0_sdr ///
         (semean) se_cum_sdr = cum_ln_price_T0_sdr, by(high_sdr relative_time)
gen ci_l_sdr = mean_cum_sdr - 1.96 * se_cum_sdr
gen ci_u_sdr = mean_cum_sdr + 1.96 * se_cum_sdr

twoway ///
    (rarea ci_l_sdr ci_u_sdr relative_time if high_sdr == 0, sort color(navy%12) lcolor(none)) ///
    (line mean_cum_sdr relative_time if high_sdr == 0, sort lcolor(navy) lwidth(medthick)) ///
    (rarea ci_l_sdr ci_u_sdr relative_time if high_sdr == 1, sort color(forest_green%12) lcolor(none)) ///
    (line mean_cum_sdr relative_time if high_sdr == 1, sort lcolor(forest_green) lwidth(medthick)), ///
    xline(0, lpattern(dash) lcolor(gs8)) ///
    yline(0, lpattern(dash) lcolor(gs10)) ///
    xtitle("相对T0的月份") ///
    ytitle("相对T0的累计房价变化（对数百分点）") ///
    title("高/低SDR城市在上涨结束点前后的房价路径") ///
    subtitle("样本：有上涨结束点T0且SDR非缺失的城市；阴影为95%置信区间") ///
    legend(order(2 "低SDR" 4 "高SDR") rows(1) position(6)) ///
    scheme(s1color) graphregion(color(white)) plotregion(color(white))

graph export "平行趋势_高低SDR_累计房价路径.png", replace
restore

reghdfe dln_price ib24.rel_shift_sdr##i.high_sdr, ///
    absorb(city_id ym) vce(cluster city_id)
est store es_highsdr

coefplot es_highsdr, ///
    keep(*.rel_shift_sdr#1.high_sdr) ///
    vertical omitted baselevels ///
    recast(connected) ciopts(recast(rcap) lcolor(forest_green%45)) ///
    mcolor(forest_green) lcolor(forest_green) msize(small) ///
    yline(0, lpattern(dash) lcolor(gs10)) ///
    xline(24, lpattern(dot) lcolor(gs8)) ///
    xlabel(, labsize(vsmall)) ///
    coeflabels( ///
        1.rel_shift_sdr#1.high_sdr  = "-24" ///
        2.rel_shift_sdr#1.high_sdr  = "-23" ///
        3.rel_shift_sdr#1.high_sdr  = "-22" ///
        4.rel_shift_sdr#1.high_sdr  = "-21" ///
        5.rel_shift_sdr#1.high_sdr  = "-20" ///
        6.rel_shift_sdr#1.high_sdr  = "-19" ///
        7.rel_shift_sdr#1.high_sdr  = "-18" ///
        8.rel_shift_sdr#1.high_sdr  = "-17" ///
        9.rel_shift_sdr#1.high_sdr  = "-16" ///
        10.rel_shift_sdr#1.high_sdr = "-15" ///
        11.rel_shift_sdr#1.high_sdr = "-14" ///
        12.rel_shift_sdr#1.high_sdr = "-13" ///
        13.rel_shift_sdr#1.high_sdr = "-12" ///
        14.rel_shift_sdr#1.high_sdr = "-11" ///
        15.rel_shift_sdr#1.high_sdr = "-10" ///
        16.rel_shift_sdr#1.high_sdr = "-9" ///
        17.rel_shift_sdr#1.high_sdr = "-8" ///
        18.rel_shift_sdr#1.high_sdr = "-7" ///
        19.rel_shift_sdr#1.high_sdr = "-6" ///
        20.rel_shift_sdr#1.high_sdr = "-5" ///
        21.rel_shift_sdr#1.high_sdr = "-4" ///
        22.rel_shift_sdr#1.high_sdr = "-3" ///
        23.rel_shift_sdr#1.high_sdr = "-2" ///
        24.rel_shift_sdr#1.high_sdr = "-1" ///
        25.rel_shift_sdr#1.high_sdr = "0" ///
        26.rel_shift_sdr#1.high_sdr = "1" ///
        27.rel_shift_sdr#1.high_sdr = "2" ///
        28.rel_shift_sdr#1.high_sdr = "3" ///
        29.rel_shift_sdr#1.high_sdr = "4" ///
        30.rel_shift_sdr#1.high_sdr = "5" ///
        31.rel_shift_sdr#1.high_sdr = "6" ///
        32.rel_shift_sdr#1.high_sdr = "7" ///
        33.rel_shift_sdr#1.high_sdr = "8" ///
        34.rel_shift_sdr#1.high_sdr = "9" ///
        35.rel_shift_sdr#1.high_sdr = "10" ///
        36.rel_shift_sdr#1.high_sdr = "11" ///
        37.rel_shift_sdr#1.high_sdr = "12" ///
        38.rel_shift_sdr#1.high_sdr = "13" ///
        39.rel_shift_sdr#1.high_sdr = "14" ///
        40.rel_shift_sdr#1.high_sdr = "15" ///
        41.rel_shift_sdr#1.high_sdr = "16" ///
        42.rel_shift_sdr#1.high_sdr = "17" ///
        43.rel_shift_sdr#1.high_sdr = "18" ///
        44.rel_shift_sdr#1.high_sdr = "19" ///
        45.rel_shift_sdr#1.high_sdr = "20" ///
        46.rel_shift_sdr#1.high_sdr = "21" ///
        47.rel_shift_sdr#1.high_sdr = "22" ///
        48.rel_shift_sdr#1.high_sdr = "23" ///
        49.rel_shift_sdr#1.high_sdr = "24") ///
    xtitle("相对T0的月份") ///
    ytitle("高SDR相对低SDR的月度涨幅差异（百分点）") ///
    title("事件研究：高SDR城市是否在T0后跌得更惨") ///
    subtitle("基准期为T0前1月；控制城市固定效应和日历年月固定效应") ///
    legend(off) ///
    scheme(s1color) graphregion(color(white)) plotregion(color(white))

graph export "事件研究_高SDR动态效应.png", replace

di "========================================================================"
di "SDR平行趋势检验：H0 = T0前24月至T0前2月的高SDR动态系数共同为0"
di "========================================================================"
matrix b = e(b)
local coefnames_sdr : colnames b
local pre_terms_sdr
forvalues s = 1/23 {
    local term "`s'.rel_shift_sdr#1.high_sdr"
    if strpos(" `coefnames_sdr' ", " `term' ") local pre_terms_sdr `pre_terms_sdr' `term'
}
if "`pre_terms_sdr'" != "" test `pre_terms_sdr'

di "========================================================================"
di "SDR事件后效应：T0至T0后24月高SDR动态系数共同为0"
di "========================================================================"
local post_terms_sdr
forvalues s = 25/49 {
    local term "`s'.rel_shift_sdr#1.high_sdr"
    if strpos(" `coefnames_sdr' ", " `term' ") local post_terms_sdr `post_terms_sdr' `term'
}
if "`post_terms_sdr'" != "" test `post_terms_sdr'

reghdfe dln_price i.post_sdr##i.high_sdr, absorb(city_id ym) vce(cluster city_id)
est store did_highsdr

reghdfe dln_price i.post_sdr##c.sdr_z, absorb(city_id ym) vce(cluster city_id)
est store did_sdrz

di "========================================================================"
di "解释：did_highsdr 中 1.post_sdr#1.high_sdr < 0 表示高SDR城市T0后月度涨幅更低。"
di "解释：did_sdrz 中 1.post_sdr#c.sdr_z < 0 表示SDR每高1个标准差，T0后月度涨幅更低。"
di "========================================================================"

* ==============================================================================
* 七：SDR版本 - 季度层面事件研究
* ==============================================================================

use `prr_month_panel', clear
keep if sdratio2_unit != .

gen q = qofd(dofm(ym))
format q %tq
gen T0_q = qofd(dofm(T0))
format T0_q %tq

collapse (mean) ln_price_q = ln_price ///
         (firstnm) high_sdr sdr_z T0_q, by(city_id q)

xtset city_id q
gen dln_price_q = 100 * (ln_price_q - L.ln_price_q)
label var dln_price_q "房价指数季度对数涨幅（%）"

gen relative_q = q - T0_q
label var relative_q "相对时间（季度，T0所在季度=0）"

// keep if inrange(relative_q, -8, 8)

bys city_id: egen ln_price_T0_q_sdr = max(cond(relative_q == 0, ln_price_q, .))
gen cum_ln_price_T0_q_sdr = 100 * (ln_price_q - ln_price_T0_q_sdr)
label var cum_ln_price_T0_q_sdr "相对T0所在季度的累计房价变化（对数百分点）"

replace relative_q = 8 if relative_q > 8
replace relative_q = -8 if relative_q < -8

gen post_q_sdr = relative_q >= 0
label var post_q_sdr "T0所在季度及之后"

gen rel_q_shift_sdr = relative_q + 9
label var rel_q_shift_sdr "平移后的相对季度（8对应T0前1季度）"

preserve
collapse (mean) mean_cum_q_sdr = cum_ln_price_T0_q_sdr ///
         (semean) se_cum_q_sdr = cum_ln_price_T0_q_sdr, by(high_sdr relative_q)
gen ci_l_q_sdr = mean_cum_q_sdr - 1.96 * se_cum_q_sdr
gen ci_u_q_sdr = mean_cum_q_sdr + 1.96 * se_cum_q_sdr

twoway ///
    (rarea ci_l_q_sdr ci_u_q_sdr relative_q if high_sdr == 0, sort color(navy%12) lcolor(none)) ///
    (line mean_cum_q_sdr relative_q if high_sdr == 0, sort lcolor(navy) lwidth(medthick)) ///
    (rarea ci_l_q_sdr ci_u_q_sdr relative_q if high_sdr == 1, sort color(forest_green%12) lcolor(none)) ///
    (line mean_cum_q_sdr relative_q if high_sdr == 1, sort lcolor(forest_green) lwidth(medthick)), ///
    xline(0, lpattern(dash) lcolor(gs8)) ///
    yline(0, lpattern(dash) lcolor(gs10)) ///
    xtitle("相对T0所在季度的季度数") ///
    ytitle("相对T0所在季度的累计房价变化（对数百分点）") ///
    title("季度层面：高/低SDR城市在上涨结束点前后的房价路径") ///
    subtitle("季度房价为季度内月度房价指数均值；阴影为95%置信区间") ///
    legend(order(2 "低SDR" 4 "高SDR") rows(1) position(6)) ///
    scheme(s1color) graphregion(color(white)) plotregion(color(white))

graph export "季度平行趋势_高低SDR_累计房价路径.png", replace
restore

reghdfe dln_price_q ib8.rel_q_shift_sdr##i.high_sdr, ///
    absorb(city_id q) vce(cluster city_id)
est store es_highsdr_q

coefplot es_highsdr_q, ///
    keep(*.rel_q_shift_sdr#1.high_sdr) ///
    vertical omitted baselevels ///
    recast(connected) ciopts(recast(rcap) lcolor(forest_green%45)) ///
    mcolor(forest_green) lcolor(forest_green) msize(small) ///
    yline(0, lpattern(dash) lcolor(gs10)) ///
    xline(8, lpattern(dot) lcolor(gs8)) ///
    xlabel(, labsize(vsmall)) ///
    coeflabels( ///
        1.rel_q_shift_sdr#1.high_sdr  = "-8" ///
        2.rel_q_shift_sdr#1.high_sdr  = "-7" ///
        3.rel_q_shift_sdr#1.high_sdr  = "-6" ///
        4.rel_q_shift_sdr#1.high_sdr  = "-5" ///
        5.rel_q_shift_sdr#1.high_sdr  = "-4" ///
        6.rel_q_shift_sdr#1.high_sdr  = "-3" ///
        7.rel_q_shift_sdr#1.high_sdr  = "-2" ///
        8.rel_q_shift_sdr#1.high_sdr  = "-1" ///
        9.rel_q_shift_sdr#1.high_sdr  = "0" ///
        10.rel_q_shift_sdr#1.high_sdr = "1" ///
        11.rel_q_shift_sdr#1.high_sdr = "2" ///
        12.rel_q_shift_sdr#1.high_sdr = "3" ///
        13.rel_q_shift_sdr#1.high_sdr = "4" ///
        14.rel_q_shift_sdr#1.high_sdr = "5" ///
        15.rel_q_shift_sdr#1.high_sdr = "6" ///
        16.rel_q_shift_sdr#1.high_sdr = "7" ///
        17.rel_q_shift_sdr#1.high_sdr = "8") ///
    xtitle("相对T0所在季度的季度数") ///
    ytitle("高SDR相对低SDR的季度涨幅差异（百分点）") ///
    title("季度事件研究：高SDR城市是否在T0后跌得更惨") ///
    subtitle("基准期为T0前1季度；控制城市固定效应和日历季度固定效应") ///
    legend(off) ///
    scheme(s1color) graphregion(color(white)) plotregion(color(white))

graph export "季度事件研究_高SDR动态效应.png", replace

di "========================================================================"
di "季度SDR平行趋势检验：H0 = T0前8季度至T0前2季度的高SDR动态系数共同为0"
di "========================================================================"
matrix b = e(b)
local coefnames_q_sdr : colnames b
local pre_terms_q_sdr
forvalues s = 1/7 {
    local term "`s'.rel_q_shift_sdr#1.high_sdr"
    if strpos(" `coefnames_q_sdr' ", " `term' ") local pre_terms_q_sdr `pre_terms_q_sdr' `term'
}
if "`pre_terms_q_sdr'" != "" test `pre_terms_q_sdr'

di "========================================================================"
di "季度SDR事件后效应：T0所在季度至T0后8季度高SDR动态系数共同为0"
di "========================================================================"
local post_terms_q_sdr
forvalues s = 9/17 {
    local term "`s'.rel_q_shift_sdr#1.high_sdr"
    if strpos(" `coefnames_q_sdr' ", " `term' ") local post_terms_q_sdr `post_terms_q_sdr' `term'
}
if "`post_terms_q_sdr'" != "" test `post_terms_q_sdr'

reghdfe dln_price_q i.post_q_sdr##i.high_sdr, absorb(city_id q) vce(cluster city_id)
est store did_highsdr_q

reghdfe dln_price_q i.post_q_sdr##c.sdr_z, absorb(city_id q) vce(cluster city_id)
est store did_sdrz_q

di "========================================================================"
di "季度解释：did_highsdr_q 中 1.post_q_sdr#1.high_sdr < 0 表示高SDR城市T0后季度涨幅更低。"
di "季度解释：did_sdrz_q 中 1.post_q_sdr#c.sdr_z < 0 表示SDR每高1个标准差，T0后季度涨幅更低。"
di "========================================================================"


di "========================================================================"
di "PRR简化DID结果汇总"
di "========================================================================"
esttab did_highprr did_prrz did_highprr_q did_prrz_q, ///
    se star(* 0.1 ** 0.05 *** 0.01) ///
    b(%9.3f) se(%9.3f) ///
    stats(N r2_a, labels("Observations" "Adj. R-squared"))

di "========================================================================"
di "SDR简化DID结果汇总"
di "========================================================================"
esttab did_highsdr did_sdrz did_highsdr_q did_sdrz_q, ///
    se star(* 0.1 ** 0.05 *** 0.01) ///
    b(%9.3f) se(%9.3f) ///
    stats(N r2_a, labels("Observations" "Adj. R-squared"))
**# 城市住房价格的下跌的影响因素分析
* ==============================================================================
* 城市层面截面分析
* ==============================================================================


cd "/Users/quanrongxi/Desktop/科研/博四/房价断点识别/任务分解/任务0523"
use sample_ym,clear
keep city province avg_gr_post avg_gr_before magni_post6 magni_post12 magni_post18 T0 region tier prr2 sdratio2_unit
duplicates drop 

*被解释变量
gen avg_gr_change =  avg_gr_post - avg_gr_before
gen year = year(dofm(T0))

gen yq = yq(year, quarter(dofm(T0)))

format yq %tq
ta yq

*控制变量
encode region ,gen(region1)
drop region
rename region1 region

encode tier,gen(tier1)
drop tier
rename tier1 tier

preserve
use  "/Volumes/硬盘/数据/宏观数据/城市宏观经济指标/城市统计年鉴_1990_2023年面板数据.dta",clear
bys 城市: gen pop_2010 = 城镇常住人口万人  if 年份 == 2010
bys 城市: gen pop_2020 = 城镇常住人口万人  if 年份 == 2020
bys 城市: fillmissing pop_2010 pop_2020
gen pop_netchange = pop_2020 - pop_2010

keep if 年份 == 2020
rename 城市 city 
drop 所属地域 省份代码 城市代码 省份 D
save 城市宏观经济属性_2020,replace

restore

// 10年-20年间净流入人口的对数
// 10-20年间住宅供给的增加

merge 1:1 city using 城市宏观经济属性_2020
keep if _merge == 3
drop _merge 

gen ln_gdp = ln(地区生产总值万元)
gen ln_pop = ln(城镇常住人口万人)
gen ln_pgdp = ln(人均地区生产总值元)
rename 第一产业增加值占GDP比重 gdp1_share
rename 第二产业增加值占GDP比重 gdp2_share
rename 第三产业增加值占GDP比重 gdp3_share

preserve
estimate clear 
local y avg_gr_post
replace `y' = `y' * 100

reg `y' prr2 ,robust
est store e1

// reg `y' sdratio2_unit,robust
// est store e2

reg `y' ln_pop ,robust
est store e3

reg `y' gdp3_share,robust
est store e4

reg `y' east ,robust
est store e5

reg `y' prr2 ln_pop gdp3_share east ,robust
est store e7

esttab e*, ///
    se star(* 0.1 ** 0.05 *** 0.01) ///
    b(%9.3f) se(%9.3f) ///
    stats(N r2_a, labels("Observations" "Adj. R-squared"))
restore




reg avg_gr_change prr2 ln_pop gdp3_share ,robust
reg year prr2 ln_pop gdp3_share,robust


teast

	
	teat
test
