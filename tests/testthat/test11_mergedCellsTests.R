library(testthat)

# most common expectations:
# equality:        expect_equal() and expect_identical()
# regexp:          expect_match()
# catch-all:       expect_true() and expect_false()
# console output:  expect_output()
# messages:        expect_message()
# warning:         expect_warning()
# errors:          expect_error()

escapeString <- function(s) {
  t <- gsub("(\\\\)", "\\\\\\\\", s)
  t <- gsub("(\n)", "\\\\n", t)
  t <- gsub("(\r)", "\\\\r", t)
  t <- gsub("(\")", "\\\\\"", t)
  return(t)
}

prepStr <- function(s, varName="html") {
  t <- escapeString(s)
  u <- eval(parse(text=paste0("\"", t, "\"")))
  if(s!=u) stop("Unable to escape string!")
  if(is.null(varName)) varName <- "html"
  t <- paste0("\t", varName, " <- \"", t, "\"")
  utils::writeClipboard(t)
  return(invisible())
}


context("MERGED CELLS TESTS")


test_that("merge cells test", {

  # data for the table
  saleIds <- c(5334, 5336, 5338, 5339)
  items <- c("Apple", "Orange", "Banana", "Grapefruit")
  quantities <- c(5, 8, 6, 2)
  prices <- c(0.34452354, 0.4732543, 1.3443243, 0.5628432)
  status <- c("Good", "OK", "Bad", "OK")

  # construct the table
  library(basictabler)
  tbl <- BasicTable$new(compatibility=list(headerCellsAsTD=TRUE))
  tbl$addData(data.frame(saleIds, items, quantities, prices, status),
              firstColumnAsRowHeaders=TRUE,
              explicitColumnHeaders=c("Sale ID", "Item", "Quantity", "Price", "Status"),
              columnFormats=list(NULL, NULL, NULL, "%.2f", NULL))

  # merge the cells and specify new heading
  tbl$mergeCells(rFrom=1, cFrom=2, rSpan=1, cSpan=2)
  cell <- tbl$cells$getCell(1, 2)
  cell$rawValue <- "Item & Qty"
  cell$formattedValue <- "Item & Qty"
  tbl$mergeCells(rFrom=3, cFrom=3, rSpan=2, cSpan=2)
  cell <- tbl$cells$getCell(3, 3)
  cell$rawValue <- "??"
  cell$formattedValue <- "??"

  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getHtml()))
  str <- "Sale ID  Item & Qty  Quantity  Price  Status  \n   5334       Apple         5   0.34    Good  \n   5336      Orange        ??   0.47      OK  \n   5338      Banana         6   1.34     Bad  \n   5339  Grapefruit         2   0.56      OK  "
  html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"ColumnHeader\">Sale ID</td>\n    <td rowspan=\"1\" colspan=\"2\" class=\"ColumnHeader\">Item &amp; Qty</td>\n    <td class=\"ColumnHeader\">Price</td>\n    <td class=\"ColumnHeader\">Status</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5334</td>\n    <td class=\"Cell\">Apple</td>\n    <td class=\"Cell\">5</td>\n    <td class=\"Cell\">0.34</td>\n    <td class=\"Cell\">Good</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5336</td>\n    <td class=\"Cell\">Orange</td>\n    <td rowspan=\"2\" colspan=\"2\" class=\"Cell\">??</td>\n    <td class=\"Cell\">OK</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5338</td>\n    <td class=\"Cell\">Banana</td>\n    <td class=\"Cell\">Bad</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5339</td>\n    <td class=\"Cell\">Grapefruit</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">0.56</td>\n    <td class=\"Cell\">OK</td>\n  </tr>\n</table>"

  expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getHtml()), html)
})



test_that("merge and manipulate test", {

  # data for the table
  ints0 <- 0:29
  ints1 <- 100:129
  ints2 <- 200:229
  ints3 <- 300:329
  ints4 <- 400:429
  ints5 <- 500:529
  ints6 <- 600:629
  ints7 <- 700:729
  ints8 <- 800:829
  ints9 <- 900:929
  df <- data.frame(ints0, ints1, ints2, ints3, ints4, ints5, ints6, ints7, ints8, ints9)
  colNames <- c("0-99", "100-199", "200-299", "300-399", "400-499", "500-599", "600-699", "700-799", "800-899", "900-999")

  # construct the table
  library(basictabler)
  tbl <- BasicTable$new(compatibility=list(headerCellsAsTD=TRUE))

  # populate the table
  tbl$addData(df, explicitColumnHeaders=colNames)

  # do some merges!
  tbl$mergeCells(rFrom=10, cFrom=4, rSpan=5, cSpan=3)
  tbl$cells$setValue(10, 4, rawValue="Merge 1")
  tbl$mergeCells(rFrom=17, cFrom=6, rSpan=7, cSpan=2)
  tbl$cells$setValue(17, 6, rawValue="Merge 2!")
  tbl$mergeCells(rFrom=6, cFrom=9, rSpan=7, cSpan=1)
  tbl$cells$setValue(6, 9, rawValue="Merge 3!")

  # do some manipulation
  tbl$cells$insertRow(4)
  tbl$cells$setValue(4, 5, rawValue="NEW")
  tbl$cells$insertRow(8)
  tbl$cells$setValue(8, 5, rawValue="NEW")
  tbl$cells$deleteRow(13)
  tbl$cells$deleteRow(12)
  tbl$cells$deleteRow(14)
  tbl$cells$deleteColumn(4)
  tbl$cells$insertColumn(6)

  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getHtml()))
  str <- "0-99  100-199  200-299  400-499   500-599    600-699  700-799   800-899  900-999  \n   0      100      200      400       500        600      700       800      900  \n   1      101      201      401       501        601      701       801      901  \n                            NEW                                                   \n   2      102      202      402       502        602      702       802      902  \n   3      103      203      403       503        603      703       803      903  \n   4      104      204      404       504        604      704  Merge 3!      904  \n                            NEW                                                   \n   5      105      205      405       505        605      705       805      905  \n   6      106      206      406       506        606      706       806      906  \n   7      107      207      407       507        607      707       807      907  \n  10      110      210      410       510        610      710       810      910  \n  11      111      211      411       511        611      711       811      911  \n  13      113      213      413       513        613      713       813      913  \n  14      114      214      414       514        614      714       814      914  \n  15      115      215      415  Merge 2!        615      715       815      915  \n  16      116      216      416       516        616      716       816      916  \n  17      117      217      417       517        617      717       817      917  \n  18      118      218      418       518        618      718       818      918  \n  19      119      219      419       519        619      719       819      919  \n  20      120      220      420       520        620      720       820      920  \n  21      121      221      421       521        621      721       821      921  \n  22      122      222      422       522        622      722       822      922  \n  23      123      223      423       523        623      723       823      923  \n  24      124      224      424       524        624      724       824      924  \n  25      125      225      425       525        625      725       825      925  \n  26      126      226      426       526        626      726       826      926  \n  27      127      227      427       527        627      727       827      927  \n  28      128      228      428       528        628      728       828      928  \n  29      129      229      429       529        629      729       829      929  "
  html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"ColumnHeader\">0-99</td>\n    <td class=\"ColumnHeader\">100-199</td>\n    <td class=\"ColumnHeader\">200-299</td>\n    <td class=\"ColumnHeader\">400-499</td>\n    <td class=\"ColumnHeader\">500-599</td>\n    <td class=\"Cell\"></td>\n    <td class=\"ColumnHeader\">600-699</td>\n    <td class=\"ColumnHeader\">700-799</td>\n    <td class=\"ColumnHeader\">800-899</td>\n    <td class=\"ColumnHeader\">900-999</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\">200</td>\n    <td class=\"Cell\">400</td>\n    <td class=\"Cell\">500</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">600</td>\n    <td class=\"Cell\">700</td>\n    <td class=\"Cell\">800</td>\n    <td class=\"Cell\">900</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">101</td>\n    <td class=\"Cell\">201</td>\n    <td class=\"Cell\">401</td>\n    <td class=\"Cell\">501</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">601</td>\n    <td class=\"Cell\">701</td>\n    <td class=\"Cell\">801</td>\n    <td class=\"Cell\">901</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">NEW</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">102</td>\n    <td class=\"Cell\">202</td>\n    <td class=\"Cell\">402</td>\n    <td class=\"Cell\">502</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">602</td>\n    <td class=\"Cell\">702</td>\n    <td class=\"Cell\">802</td>\n    <td class=\"Cell\">902</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">103</td>\n    <td class=\"Cell\">203</td>\n    <td class=\"Cell\">403</td>\n    <td class=\"Cell\">503</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">603</td>\n    <td class=\"Cell\">703</td>\n    <td class=\"Cell\">803</td>\n    <td class=\"Cell\">903</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">104</td>\n    <td class=\"Cell\">204</td>\n    <td class=\"Cell\">404</td>\n    <td class=\"Cell\">504</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">604</td>\n    <td class=\"Cell\">704</td>\n    <td rowspan=\"6\" colspan=\"1\" class=\"Cell\">Merge 3!</td>\n    <td class=\"Cell\">904</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">NEW</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">5</td>\n    <td class=\"Cell\">105</td>\n    <td class=\"Cell\">205</td>\n    <td class=\"Cell\">405</td>\n    <td class=\"Cell\">505</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">605</td>\n    <td class=\"Cell\">705</td>\n    <td class=\"Cell\">905</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">6</td>\n    <td class=\"Cell\">106</td>\n    <td class=\"Cell\">206</td>\n    <td class=\"Cell\">406</td>\n    <td class=\"Cell\">506</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">606</td>\n    <td class=\"Cell\">706</td>\n    <td class=\"Cell\">906</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">7</td>\n    <td class=\"Cell\">107</td>\n    <td class=\"Cell\">207</td>\n    <td class=\"Cell\">407</td>\n    <td class=\"Cell\">507</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">607</td>\n    <td class=\"Cell\">707</td>\n    <td class=\"Cell\">907</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">10</td>\n    <td class=\"Cell\">110</td>\n    <td class=\"Cell\">210</td>\n    <td rowspan=\"2\" colspan=\"2\" class=\"Cell\">410</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">610</td>\n    <td class=\"Cell\">710</td>\n    <td class=\"Cell\">910</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">11</td>\n    <td class=\"Cell\">111</td>\n    <td class=\"Cell\">211</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">611</td>\n    <td class=\"Cell\">711</td>\n    <td class=\"Cell\">811</td>\n    <td class=\"Cell\">911</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">13</td>\n    <td class=\"Cell\">113</td>\n    <td class=\"Cell\">213</td>\n    <td class=\"Cell\">413</td>\n    <td class=\"Cell\">513</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">613</td>\n    <td class=\"Cell\">713</td>\n    <td class=\"Cell\">813</td>\n    <td class=\"Cell\">913</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">14</td>\n    <td class=\"Cell\">114</td>\n    <td class=\"Cell\">214</td>\n    <td class=\"Cell\">414</td>\n    <td class=\"Cell\">514</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">614</td>\n    <td class=\"Cell\">714</td>\n    <td class=\"Cell\">814</td>\n    <td class=\"Cell\">914</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">15</td>\n    <td class=\"Cell\">115</td>\n    <td class=\"Cell\">215</td>\n    <td class=\"Cell\">415</td>\n    <td rowspan=\"7\" colspan=\"3\" class=\"Cell\">Merge 2!</td>\n    <td class=\"Cell\">715</td>\n    <td class=\"Cell\">815</td>\n    <td class=\"Cell\">915</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">16</td>\n    <td class=\"Cell\">116</td>\n    <td class=\"Cell\">216</td>\n    <td class=\"Cell\">416</td>\n    <td class=\"Cell\">716</td>\n    <td class=\"Cell\">816</td>\n    <td class=\"Cell\">916</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">17</td>\n    <td class=\"Cell\">117</td>\n    <td class=\"Cell\">217</td>\n    <td class=\"Cell\">417</td>\n    <td class=\"Cell\">717</td>\n    <td class=\"Cell\">817</td>\n    <td class=\"Cell\">917</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">18</td>\n    <td class=\"Cell\">118</td>\n    <td class=\"Cell\">218</td>\n    <td class=\"Cell\">418</td>\n    <td class=\"Cell\">718</td>\n    <td class=\"Cell\">818</td>\n    <td class=\"Cell\">918</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">19</td>\n    <td class=\"Cell\">119</td>\n    <td class=\"Cell\">219</td>\n    <td class=\"Cell\">419</td>\n    <td class=\"Cell\">719</td>\n    <td class=\"Cell\">819</td>\n    <td class=\"Cell\">919</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">20</td>\n    <td class=\"Cell\">120</td>\n    <td class=\"Cell\">220</td>\n    <td class=\"Cell\">420</td>\n    <td class=\"Cell\">720</td>\n    <td class=\"Cell\">820</td>\n    <td class=\"Cell\">920</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">21</td>\n    <td class=\"Cell\">121</td>\n    <td class=\"Cell\">221</td>\n    <td class=\"Cell\">421</td>\n    <td class=\"Cell\">721</td>\n    <td class=\"Cell\">821</td>\n    <td class=\"Cell\">921</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">22</td>\n    <td class=\"Cell\">122</td>\n    <td class=\"Cell\">222</td>\n    <td class=\"Cell\">422</td>\n    <td class=\"Cell\">522</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">622</td>\n    <td class=\"Cell\">722</td>\n    <td class=\"Cell\">822</td>\n    <td class=\"Cell\">922</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">23</td>\n    <td class=\"Cell\">123</td>\n    <td class=\"Cell\">223</td>\n    <td class=\"Cell\">423</td>\n    <td class=\"Cell\">523</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">623</td>\n    <td class=\"Cell\">723</td>\n    <td class=\"Cell\">823</td>\n    <td class=\"Cell\">923</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">24</td>\n    <td class=\"Cell\">124</td>\n    <td class=\"Cell\">224</td>\n    <td class=\"Cell\">424</td>\n    <td class=\"Cell\">524</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">624</td>\n    <td class=\"Cell\">724</td>\n    <td class=\"Cell\">824</td>\n    <td class=\"Cell\">924</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">25</td>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\">225</td>\n    <td class=\"Cell\">425</td>\n    <td class=\"Cell\">525</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">625</td>\n    <td class=\"Cell\">725</td>\n    <td class=\"Cell\">825</td>\n    <td class=\"Cell\">925</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">26</td>\n    <td class=\"Cell\">126</td>\n    <td class=\"Cell\">226</td>\n    <td class=\"Cell\">426</td>\n    <td class=\"Cell\">526</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">626</td>\n    <td class=\"Cell\">726</td>\n    <td class=\"Cell\">826</td>\n    <td class=\"Cell\">926</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">27</td>\n    <td class=\"Cell\">127</td>\n    <td class=\"Cell\">227</td>\n    <td class=\"Cell\">427</td>\n    <td class=\"Cell\">527</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">627</td>\n    <td class=\"Cell\">727</td>\n    <td class=\"Cell\">827</td>\n    <td class=\"Cell\">927</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">28</td>\n    <td class=\"Cell\">128</td>\n    <td class=\"Cell\">228</td>\n    <td class=\"Cell\">428</td>\n    <td class=\"Cell\">528</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">628</td>\n    <td class=\"Cell\">728</td>\n    <td class=\"Cell\">828</td>\n    <td class=\"Cell\">928</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">29</td>\n    <td class=\"Cell\">129</td>\n    <td class=\"Cell\">229</td>\n    <td class=\"Cell\">429</td>\n    <td class=\"Cell\">529</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">629</td>\n    <td class=\"Cell\">729</td>\n    <td class=\"Cell\">829</td>\n    <td class=\"Cell\">929</td>\n  </tr>\n</table>"

  expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getHtml()), html)
})
