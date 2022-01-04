instrumentPtsd20En = function(mhiraData){ 
print("instrumentPtsd20En")
#Packages ----
library(tidyverse)


# Settings ----   
  
# This script works for these questionnaires
Quest =  c("Ptsd20En")

data = mhiraData

if (!any(data$questionnaireName %in%  Quest)){return(NULL)}

#
# Rearragne data ----

df = data %>% select(name, questionnaireName, textValue, assessmentName) %>% pivot_wider(id_cols = c(assessmentName, questionnaireName), names_from = name, values_from = textValue)
df = df %>% filter(questionnaireName == Quest) %>% discard(~all(is.na(.)))
df = df %>% mutate_at(.vars = vars(starts_with("PTSD")), .funs = as.numeric )

# Item patterns ----
cluster_B <- c("PTSD1", "PTSD2", "PTSD3", "PTSD4", "PTSD5")
cluster_C <- c("PTSD6", "PTSD7")
cluster_D <- c("PTSD8", "PTSD9", "PTSD10", "PTSD11", "PTSD12", "PTSD13", "PTSD14")
cluster_E <- c("PTSD15", "PTSD16", "PTSD17", "PTSD18", "PTSD19", "PTSD20")
total <- c("PTSD1", "PTSD2", "PTSD3", "PTSD4", "PTSD5", "PTSD6", "PTSD7", "PTSD8", "PTSD9",
           "PTSD10", "PTSD11", "PTSD12", "PTSD13", "PTSD14", "PTSD15", "PTSD16", "PTSD17",
           "PTSD18", "PTSD19", "PTSD20")

# Scales  

df = df %>% select(assessmentName, questionnaireName, starts_with("PTSD"))

dfS = data.frame(
df %>% select(all_of(cluster_B)) %>% summarise(rowSums(.)), 
df %>% select(all_of(cluster_C)) %>% summarise(rowSums(.)), 
df %>% select(all_of(cluster_D)) %>% summarise(rowSums(.)),
df %>% select(all_of(cluster_E)) %>% summarise(rowSums(.)),
df %>% select(all_of(total)) %>% summarise(rowSums(.)) 
)

colnames(dfS) = c('cluster_B', 'cluster_C', 'cluster_D', 'cluster_E', 'total')

dfS$cluster_B_t = 4 * dfS$cluster_B
dfS$cluster_C_t = 10 * dfS$cluster_C
dfS$cluster_D_t = 20/7 * dfS$cluster_D
dfS$cluster_E_t = 20/6 * dfS$cluster_E
dfS$total_t = dfS$total

dfS = data.frame(assessmentName = df$assessmentName, questionnaireName = df$questionnaireName , dfS)

dfS = dfS %>% select(assessmentName,questionnaireName, ends_with("_t")) %>%
  na.omit() %>%
  pivot_longer(values_to = "value", names_to = "variable", cols = ends_with("_t"))

# dfS$text = as.character(NA)
dfS$assessmentDate = NA
dfS$topic = "Post-Traumatic Stress Disorder"
dfS$severity = NA
dfS$Recommendation = NA
dfS$Assessment = NA
dfS$Warning = NA

df_diag = df > 1

df$diag = prod(df_diag[3:7]) * prod(df_diag[8:9]) * prod(df_diag[10:16]) * prod(df_diag[17:22])

diag_template = 'Based on the answers given to the PTSD Checklist for DSM-5 the patient an be considered to xFx.'

dfS$diag = factor(df$diag, levels = c(0:1), labels =c("not exhibit symptoms of PTSD", "exhibit symptoms of PTSD"))

dfS$Warning[5] = ifelse(is.na(dfS$diag[5]), NA, sub("xFx", dfS$diag[5], diag_template))

dfS$Recommendation[dfS$variable == "total_t"] = "If levels of total post-traumatic stress disorder are pathological (above 31), please consult a mental health care professional." 
dfS$Assessment[dfS$variable == "total_t"] = "Please consider the figure for level of severity"

cut_offs = tibble(topic = dfS$topic[1],
                      questionnaireName = dfS$questionnaireName[1],
                      level = factor(c('normal', 'severe'), levels = c('normal', 'severe')),
                      low_cut = c(0,31), high_cut = c(31,80))


return_list = list(values = dfS, cut_offs =  cut_offs)

return(return_list)

}
