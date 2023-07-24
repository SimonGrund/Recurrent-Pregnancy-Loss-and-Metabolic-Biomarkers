library(data.table)
library(tidyverse)

files = list.files("../Results/Tables/TMP/", full.names = T)

for(f in files){
  tmp = fread(f)
  tmp_out = tmp
  if(f == files[1]){
    out = tmp_out
  }else{
    out = bind_rows(out, tmp_out)
  }
}

out
out = as.data.frame(out)
openxlsx::write.xlsx(out, "../Results/Tables/Table3.xlsx")
