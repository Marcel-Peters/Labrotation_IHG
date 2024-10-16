#> 

#install.packages("ontologyIndex","tidyverse")
#Sys.setenv(LANG = "en")

#Hp.Obo MUST be provided. It can be downloaded from the website: https://obofoundry.org/ontology/hp.html. It contains lots of good stuff.
hpo_obo <- "G:/Labrotation_Thesis/R/resources/hp.obo"
# path_to_Excel <- "G:/Labrotation final/Labrotation_for_R_final.xlsx"
path_to_filtered_tsv <- "G:/Labrotation_thesis/R/resources/annotated_Prefiltered_to_tsv.tsv" #"C:/Labrotation/snakemake_vep/results/filtering/annotated_Prefiltered_to_tsv.tsv"
path_to_panel_neurodevelopmental <- "G:/Labrotation_thesis/R/resources/Intellectual disability - microarray and sequencing_v5.0.tsv" #"C:/Labrotation/snakemake_vep/Intellectual disability - microarray and sequencing_v5.0.tsv"

library(tidyverse)
plot.directory <- "G:/Labrotation_Thesis/R Plots/"

#Tutorials
#Other Basics: https://datacarpentry.org/semester-biology/materials/for-loops-R/
#Other Basics: https://www.datamentor.io/r-programming/vector

#Make a more convoluted piechart:
#https://www.youtube.com/watch?v=1mJh0Cb4t4M&ab_channel=RVStatsConsulting
#https://www.youtube.com/watch?v=c_IUAw4T3L4&ab_channel=RiffomonasProject
#https://r-graph-gallery.com/piechart-ggplot2.html
#https://stackoverflow.com/questions/26748069/ggplot2-pie-and-donut-chart-on-same-plot 


# 0. Import information from Excel Sheets ---------------------------------

#THE EXCEL SHOULD BE BROUGHT ON AN EXTERIOR DRIVE AND MUST LIE IN A SUBFOLDER NAMED R AND ITSELF
#BE NAMED Labrotation_preparation_for_R_final.xlsx

#Read and import the Excel from the Datasheet.
#NOTE: The old file was: "D:/R/Labrotation_preparation_for_R_final.xlsx"
#sheet = "All Variants")

library(readxl)
Logistic_and_Metadata <- read_excel("G:/Labrotation_Thesis/R/resources/Labrotation_for_R_final.xlsx", 
                                    sheet = "Combined Table")
Logistic_and_Metadata_old <- read_excel("G:/Labrotation_Thesis/R/resources/Labrotation_for_R_2506.xlsx", 
                                    sheet = "Combined Table")
Diagnostics_Variants_found <- read_excel("G:/Labrotation_Thesis/R/resources/Labrotation_for_R_final.xlsx", 
                                         sheet = "All Variants, updated, fixed")
Diagnostics_Variants_found_old <- read_excel("G:/Labrotation_Thesis/R/resources/Labrotation_for_R_2506.xlsx", 
                                         sheet = "All_Variants")

#Save a backup of unprocessed dataframes

Logistic_and_Metadata_backup <- Logistic_and_Metadata
Logistic_and_Metadata_old_backup <- Logistic_and_Metadata_old
Diagnostics_Variants_found_backup <- Diagnostics_Variants_found
Diagnostics_Variants_found_old_backup <- Diagnostics_Variants_found_old

# 0.1 Preprocessing: Deleting Headers and duplicates in Variants and in Logistic and Metadata----------
#First lines of the dataframes were just for orientation in regards to the original Excels-data
Logistic_and_Metadata <- Logistic_and_Metadata[-1, ]
Logistic_and_Metadata_old <- Logistic_and_Metadata_old[-1, ]
Diagnostics_Variants_found <- Diagnostics_Variants_found[-1, ]
Diagnostics_Variants_found_old <- Diagnostics_Variants_found_old[-1, ]

#Removing duplicates
#By default, duplicated will give FALSE for the first occurrence of anything, and TRUE for every other occurrence. 
#Setting fromLast = TRUE will make reverse the direction, so that ALL non-duplicated entries, as well as the last (most recent) occurrence of duplicates is kept due to the inversion of logical values by !
#Note - This will work as intended, if we have more than 2 duplicates.

Logistic_and_Metadata <- Logistic_and_Metadata[!duplicated(Logistic_and_Metadata$`Personen-ID`, fromLast = TRUE), ]
#Labreport: 45 Duplicates removed
Logistic_and_Metadata_old <- Logistic_and_Metadata_old[!duplicated(Logistic_and_Metadata_old$`Personen-ID`, fromLast = TRUE), ]
#Labreport: 45 Duplicates removed

#Included failsafe
Diagnostics_Variants_found <- Diagnostics_Variants_found[!duplicated(Diagnostics_Variants_found$`Variant-table-Zeile`, fromLast = TRUE), ]
Diagnostics_Variants_found_old <- Diagnostics_Variants_found_old[!duplicated(Diagnostics_Variants_found_old$`Variant-table-Zeile`, fromLast = TRUE), ]

# 0.2 Preprocessing: Unify Disease Categories, HPO terms and Case Status ----------------------------
# 0.2.1  Unify terms for disease categories
#The order in which we will be doing that will be:
#1. Neurodevelopmental
#2. organ abnormality
#3. Neurological/neuromuscular
#4. cardiovascular
#5. Haematopoiesis / Immune System
#6. Incomplete Info (Na and 0)

#NOTE: THE ABOVE IS THE HIERARCHICAL ORDER, IN WHICH THE DATA WILL BE UNIFIED, MEANING ANY DOUBLES (e.g. "Neurodevelopmental, cardiovascular") WILL BE UNIFIED TO ONLY ONE CATEGORY (In this case to "neurodevelopmental")
#This is good anyways, because we don't even want those doubles at all.

#The following code uses grepl("(?i)neurode", which is case INSENSITIVE and will more or less autocomplete and select any string, which contains [...]"neurode"[...]
#If the grepl finds something, it is overwritten by the string given. ELSE the disease category, which already exists in the dataframe is used.

#After 
#Unify 1. Neurodevelopmental
Logistic_and_Metadata$`disease category` <- ifelse(grepl("(?i)neurode", Logistic_and_Metadata$`disease category`), "neurodevelopmental", Logistic_and_Metadata$`disease category`)
#Unify 2. organ abnormality
Logistic_and_Metadata$`disease category` <- ifelse(grepl("(?i)organ", Logistic_and_Metadata$`disease category`), "organ abnormality", Logistic_and_Metadata$`disease category`)
#Unify 3. Neurological/neuromuscular
Logistic_and_Metadata$`disease category` <- ifelse(grepl("(?i)neurolo", Logistic_and_Metadata$`disease category`), "neurological / neuromuscular", Logistic_and_Metadata$`disease category`)
#Unify 4. cardiovascular
Logistic_and_Metadata$`disease category` <- ifelse(grepl("(?i)cardio", Logistic_and_Metadata$`disease category`), "cardiovascular", Logistic_and_Metadata$`disease category`)
#Unify 5. Haematopoiesis / Immune System
Logistic_and_Metadata$`disease category` <- ifelse(grepl("(?i)haemato", Logistic_and_Metadata$`disease category`) | grepl("(?i)immune", Logistic_and_Metadata$`disease category`), "haematopoiesis / immune system", Logistic_and_Metadata$`disease category`)
#Unify 6. Incomplete Info (Na and 0)
Logistic_and_Metadata$`disease category` <- ifelse(is.na(Logistic_and_Metadata$`disease category`) | Logistic_and_Metadata$`disease category` == 0, NA, Logistic_and_Metadata$`disease category`)

#Before 
#Unify 1. Neurodevelopmental
Logistic_and_Metadata_old$`disease category` <- ifelse(grepl("(?i)neurode", Logistic_and_Metadata_old$`disease category`), "neurodevelopmental", Logistic_and_Metadata_old$`disease category`)
#Unify 2. organ abnormality
Logistic_and_Metadata_old$`disease category` <- ifelse(grepl("(?i)organ", Logistic_and_Metadata_old$`disease category`), "organ abnormality", Logistic_and_Metadata_old$`disease category`)
#Unify 3. Neurological/neuromuscular
Logistic_and_Metadata_old$`disease category` <- ifelse(grepl("(?i)neurolo", Logistic_and_Metadata_old$`disease category`), "neurological / neuromuscular", Logistic_and_Metadata_old$`disease category`)
#Unify 4. cardiovascular
Logistic_and_Metadata_old$`disease category` <- ifelse(grepl("(?i)cardio", Logistic_and_Metadata_old$`disease category`), "cardiovascular", Logistic_and_Metadata_old$`disease category`)
#Unify 5. Haematopoiesis / Immune System
Logistic_and_Metadata_old$`disease category` <- ifelse(grepl("(?i)haemato", Logistic_and_Metadata_old$`disease category`) | grepl("(?i)immune", Logistic_and_Metadata_old$`disease category`), "haematopoiesis / immune system", Logistic_and_Metadata_old$`disease category`)
#Unify 6. Incomplete Info (Na and 0)
Logistic_and_Metadata_old$`disease category` <- ifelse(is.na(Logistic_and_Metadata_old$`disease category`) | Logistic_and_Metadata_old$`disease category` == 0, NA, Logistic_and_Metadata_old$`disease category`)

#0.2.3 Unify incomplete Info to NA for HPO terms & Case Status --------

#Unify HPO-terms of entries with "NA" and "0" to NA
Logistic_and_Metadata$`HPO terms` <- ifelse(is.na(Logistic_and_Metadata$`HPO terms`) | Logistic_and_Metadata$`HPO terms` == 0, NA, Logistic_and_Metadata$`HPO terms`)
Logistic_and_Metadata_old$`HPO terms` <- ifelse(is.na(Logistic_and_Metadata_old$`HPO terms`) | Logistic_and_Metadata_old$`HPO terms` == 0, NA, Logistic_and_Metadata_old$`HPO terms`)

#Unify Case-status of entries with "NA" and "0" to NA
Logistic_and_Metadata$`case status` <- ifelse(is.na(Logistic_and_Metadata$`case status`) | Logistic_and_Metadata$`case status` == 0, NA, Logistic_and_Metadata$`case status`)
Logistic_and_Metadata_old$`case status` <- ifelse(is.na(Logistic_and_Metadata_old$`case status`) | Logistic_and_Metadata_old$`case status` == 0, NA, Logistic_and_Metadata_old$`case status`)

# 0.3 Preprocessing: Make a combined dataframe----------

#Merged them into All_info_if_case_has_variant
All_info_if_case_has_diagnostic_variant<-merge(Logistic_and_Metadata, Diagnostics_Variants_found, c="Position in Exom-Table")


# 1. Analysis of the databank before and after the Labrotation--------------------------------------------
#Aim of this section is to show the improvements made to the data kept in the databank from the beginning of the Labrotation to the end.

# 1.1 Assessment: Variants with incomplete Annotation before and after--------
#The most reliable indicator for complete annotations was gauged manually.
#The column `HGVS_cDNA` was chosen

#Unify nomenclautre of entries with "NA" and "0" to NA
Diagnostics_Variants_found$`HGVS_cDNA` <- ifelse(is.na(Diagnostics_Variants_found$`HGVS_cDNA`) | Diagnostics_Variants_found$`HGVS_cDNA` == 0, NA, Diagnostics_Variants_found$`HGVS_cDNA`)
Diagnostics_Variants_found_old$`HGVS_cDNA` <- ifelse(is.na(Diagnostics_Variants_found_old$`HGVS_cDNA`) | Diagnostics_Variants_found_old$`HGVS_cDNA` == 0, NA, Diagnostics_Variants_found_old$`HGVS_cDNA`)

# Visualise the development using ggplot 
# Create a new dataframe for plotting

plot_before_and_after_variants <- data.frame(
  group = c("beginning of study", "beginning of study", "end of study", "end of study"),
  order = c(1,2,3,4),
  category = c("total recorded variants", "incomplete annotation", "total recorded variants", "incomplete annotation"),
  count = c(
    nrow(Diagnostics_Variants_found_old),
    sum(is.na(Diagnostics_Variants_found_old$`HGVS_cDNA`)),
    nrow(Diagnostics_Variants_found),
    sum(is.na(Diagnostics_Variants_found$`HGVS_cDNA`))
  )
)

# Create the box plot

#ggplot(plot_before_and_after_variants, aes(x = reorder(group, order), y = count, fill = reorder(category, order))) +
#    geom_bar(stat = "identity", position = "dodge") +
#    geom_text(aes(label = count), vjust = -0.5, position = position_dodge(0.9)) +
#    scale_fill_manual(values = palette.colors(palette = "Okabe-Ito")) +
#    labs(title = "Comparison of Total and Missing Variants Before and After",
#         x = "Category",
#         y = "Count",
#         fill = NULL) +  # Remove the title of the legend
#    scale_y_continuous(limits = c(0, 300)) +
#    theme_minimal()

#Make a function for creating before and after boxplots
draw_plot_before_after <- function(dataframe, plot_title, limit_y, title_x, title_y, width_png, height_png, labs_fill = NULL) {
    # Create the box plot
    plot <- ggplot(dataframe, aes(x = reorder(group, order), y = count, fill = reorder(category, order))) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(aes(label = count), vjust = -0.5, position = position_dodge(0.9)) +
        scale_fill_manual(values = palette.colors(palette = "Okabe-Ito")) +
        labs(
            title = plot_title,
            x = title_x,
            y = title_y,
            fill = labs_fill) +  # Remove the title of the legend 
        scale_y_continuous(limits = c(0, limit_y)) +
        theme_minimal() +
        theme(
            plot.title = element_text(hjust = 0.5, size = 13),
            axis.title.y = element_text(size = 11)  # Make the y-axis title smaller
        )

    # Print the plot
    print(plot)

    # Save the plot
    ggsave(filename = paste0(plot.directory, "Chapter_1_Before_After_", plot_title, ".png"), plot = plot, width = width_png, height = 4, dpi = 300)
}

#Uses the created function for comparing the variants
draw_plot_before_after(plot_before_and_after_variants, plot_title = "variant entries in databank", limit_y = 300, title_x = "", title_y = "Count",width_png=5, height_png= 4)

# 1.2 Assessment: Incomplete Case-Status, HPO-terms & disease categories before and after--------


#1.2.1 Prepare dataframe --------

plot_before_and_after_HPO_terms_and_disease_category_and_case_status <- data.frame(
    #group = c(rep("beginning of study", 4), rep("end of study", 4)),
    #order = 1:8,
    #category = rep(c("total clinical cases", "missing HPO-terms", "missing disease category", "missing case status"), 2),
    
  group = c(rep("beginning of study", 3), rep("end of study", 3)),
  order = 1:6,
  category = rep(c("missing HPO-terms", "missing disease category", "missing case status"), 2),
#Excluded total count of clinical cases, because it is less relevant for the comparison
    count = c(
#        nrow(Logistic_and_Metadata),
        sum(is.na(Logistic_and_Metadata$`HPO terms`)),
        sum(is.na(Logistic_and_Metadata$`disease category`)),
        sum(is.na(Logistic_and_Metadata$`case status`)),

#        nrow(Logistic_and_Metadata_old),
        sum(is.na(Logistic_and_Metadata_old$`HPO terms`)),
        sum(is.na(Logistic_and_Metadata_old$`disease category`)),
        sum(is.na(Logistic_and_Metadata_old$`case status`))
    )
)
#1.2.2 Draw plot --------

draw_plot_before_after(plot_before_and_after_HPO_terms_and_disease_category_and_case_status, plot_title = "Metadata in databank", limit_y =50, title_x = "", title_y = "Count",width_png=8, height_png= 4 )


# 1.3 Assessment: How many variants per case ----------------

#Erstellt eine Tabelle aus 1. Anzahl der Varianten pro Patient, 2. Anzahl der Varianten insgesamt pro bracket, 3. Anzahl der Patienten pro Bracket.
solv_or_nah_df <- Diagnostics_Variants_found %>%
  group_by(`Position in Exom-Table`)%>%
  mutate(num_variants_per_patient=n())%>%
  group_by(num_variants_per_patient)%>%
  mutate(Patients_per_bracket = n_distinct(`Position in Exom-Table`))%>%
  group_by(num_variants_per_patient)%>%
  mutate(variants_identified = n())%>%
  distinct(num_variants_per_patient, variants_identified, Patients_per_bracket)     

solv_or_nah_df_old <- Diagnostics_Variants_found_old %>%
  group_by(`Position in Exom-Table`)%>%
  mutate(num_variants_per_patient=n())%>%
  group_by(num_variants_per_patient)%>%
  mutate(Patients_per_bracket = n_distinct(`Position in Exom-Table`))%>%
  group_by(num_variants_per_patient)%>%
  mutate(variants_identified = n())%>%
  distinct(num_variants_per_patient, variants_identified, Patients_per_bracket)     

#Example-Output:
#num_variants_per_patient variants_identified Patients_per_bracket
#<int>               <int>                <int>
#  1                        1                 208                  208
#2                        2                  32                   16
#3                        4                   4                    1
#4                        3                   9                    3

# 1.3.1 Assessment: Make the dataframe ----------------

plot_before_and_after_var_per_case <- data.frame(
  group = c(rep("beginning of study", nrow(solv_or_nah_df_old)), rep("end of study", nrow(solv_or_nah_df))),
  order = c(1, 2, 3, 4, 10, 9, 8, 7, 6, 5),
  category = c(solv_or_nah_df_old$num_variants_per_patient, solv_or_nah_df$num_variants_per_patient),
  count = c(solv_or_nah_df_old$Patients_per_bracket, solv_or_nah_df$Patients_per_bracket)
)

# 1.3.2 Assessment: Make the plot ----------------

draw_plot_before_after(plot_before_and_after_var_per_case, plot_title = "variants per clinical case", limit_y =350, title_x = "", title_y = "Count",width_png=8, height_png= 4, labs_fill="variants per case")


# 1.4 Summary-of-solved/unsolved/other -------------------------------------

# 1.4.1 Insert a "Variant-Found" Column----------

#Plan:
#1. Add column to the Logistics table.
#2. Insert FALSE into each cell of the column
#3. If Variant found "Position in Exom-Table" Matches any line in the logistics table, then Change the column there into TRUE.

#Step 1+2
#Make a namevector (Vector with the names of the new columns)
namevector <- "Variant found"
Logistic_and_Metadata[, namevector] <- FALSE #or NA
Logistic_and_Metadata_old[, namevector] <- FALSE

#Step 3:
#If the value of a cell in column 2 "Position in Exom-Table" Matches that of an element in column 1 in Diagnostics_variants_found, then Change it to TRUE
#make another helper Vector. This time, it notes down the ID (row ID, if the value within one of the cells in column "Position in Exom-Table" matches the value of any cell in column "$`Position in Exom-Table`" found in Diagnostics_variants_found  )
Idx <- match(Logistic_and_Metadata$`Position in Exom-Table`, Diagnostics_Variants_found$`Position in Exom-Table`)
Idx_old <- match(Logistic_and_Metadata_old$`Position in Exom-Table`, Diagnostics_Variants_found_old$`Position in Exom-Table`)

#Trim the vector, so all the no-matches = NA are excluded
idxn <- which(!is.na(Idx))
idxn_old <- which(!is.na(Idx_old))

#Changes every value of the column "Variant found" for the positions given in the vector [idxn] into TRUE
Logistic_and_Metadata$`Variant found`[idxn] <- TRUE
Logistic_and_Metadata_old$`Variant found`[idxn_old] <- TRUE



# 1.4.2 Creating the Dataframe -------------------------------------

#Makes a dataframe out of all the Logistic and Metadata. 
#Counts all rows, where the "case status" && "variant found" are the same. 
#E.g. counts all case status = solved && variant found == TRUE in one cell of the data.frame which is evoked.
#NOTE: For easier graphical analysis the case status of NA will be changed to "no entry"
#NOTE: For easier graphical analysis a row with for partially solved cases without any found variant was added.

df_case_status_with_variant_found <- data.frame(Logistic_and_Metadata %>%
                   count(`case status`, `Variant found`, sort =TRUE))
df_case_status_with_variant_found$case.status <- ifelse(is.na(df_case_status_with_variant_found$case.status), "no entry", df_case_status_with_variant_found$case.status)
df_case_status_with_variant_found <- df_case_status_with_variant_found %>%
  add_row(case.status="partially solved", Variant.found=FALSE, n=0) %>%
  add_column(Before_or_after = "After")

df_case_status_with_variant_found_old <- data.frame(Logistic_and_Metadata_old %>%
                   count(`case status`, `Variant found`, sort =TRUE))
df_case_status_with_variant_found_old$case.status <- ifelse(is.na(df_case_status_with_variant_found_old$case.status), "no entry", df_case_status_with_variant_found_old$case.status)
df_case_status_with_variant_found_old <- df_case_status_with_variant_found_old %>%
  add_row(case.status="partially solved", Variant.found=FALSE, n=0) %>%
  add_column(Before_or_after = "Before")

#1.4.2.1 Including Positions for the stacked plot
#Calculate appropiate positions for labels in the stacked bar graphs
df_case_status_with_variant_found <- df_case_status_with_variant_found %>%
  group_by(case.status) %>%
  arrange(desc(Variant.found)) %>%
  mutate(pos = cumsum(n) - n / 2)

df_case_status_with_variant_found_old <- df_case_status_with_variant_found_old %>%
  group_by(case.status) %>%
  arrange(desc(Variant.found)) %>%
  mutate(pos = cumsum(n) - n / 2)



#Makes colors for the stacked bars
#Uses select colors from the Ikabe-Ito Color palette
colors.solved.unsolved <- c("TRUE" = "#009E73", "FALSE" = "#E69F00", "NA"="#000000")


#1.4.3 Plotting the Stacked-Bar-Graph------------------

#Future improvement: the two following ones could be grouped together, by writing a function, which works the same for both, but takes an argument for whether you want to have stacked (=> position_stack) or grouped columns (=> position_dodge)
#Create the stacked bar plot while excluding all the NA rows from the original data.frame.

plot <- ggplot() +
        scale_fill_manual(values = colors.solved.unsolved) +
        geom_bar(data = df_case_status_with_variant_found, 
        mapping = aes(x = reorder(factor(case.status), -n), y = n, fill = Variant.found ), 
        stat = "identity", 
        position='stack',
        width = 0.8) +
        geom_text(data = df_case_status_with_variant_found,
            aes(
                x = reorder(factor(case.status), -n),
                y = pos,
                label = ifelse(n <= 2, "", n)),
                color = "white") +
        geom_text(data = df_case_status_with_variant_found %>% group_by(case.status) %>% summarise(total = sum(n)),
            aes(x = reorder(factor(case.status), -total), y = total, label = total),
                vjust = -0.5, color = "black") +
    labs(title = "end of study",x = "case status", y = "number of cases", fill = "variant entry in databank") + 
    scale_y_continuous(limits = c(0, 125)) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 13),
        axis.title.y = element_text(size = 11),
        axis.title.x = element_text(size = 11))
    # Print the plot
    print(plot)

    # Save the plot
    ggsave(filename = paste0(plot.directory, "Chapter_1_Before_or_after_Variant-entry in databank.png"), plot = plot, width = 6, height = 4, dpi = 300)

rm(plot)


plot <- ggplot() +
        scale_fill_manual(values = colors.solved.unsolved) +
        geom_bar(data = df_case_status_with_variant_found_old, 
        mapping = aes(x = reorder(factor(case.status), -n), y = n, fill = Variant.found ), 
        stat = "identity", 
        position='stack',
        width = 0.8) +
        geom_text(data = df_case_status_with_variant_found_old,
            aes(
                x = reorder(factor(case.status), -n),
                y = pos,
                label = ifelse(n <= 2, "", n)),
                color = "white") +
        geom_text(data = df_case_status_with_variant_found_old %>% group_by(case.status) %>% summarise(total = sum(n)),
            aes(x = reorder(factor(case.status), -total), y = total, label = total),
                vjust = -0.5, color = "black") +
    labs(title = "beginning of study",x = "case status", y = "number of cases", fill = "variant entry in Databank") +
    scale_y_continuous(limits = c(0, 125)) +
    theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5, size = 13),
        axis.title.y = element_text(size = 11),
        axis.title.x = element_text(size = 11))
    # Print the plot
    print(plot)

    # Save the plot
    ggsave(filename = paste0(plot.directory, "Chapter_1_Before_or_after_Variant-entry in databank before.png"), plot = plot, width = 6, height = 4, dpi = 300)
    
rm(plot)


# 2. Cohort Studies -------------------------------------------------------

# 2.1 Questions - What does my Cohort look like? --------------------------

#Only the improved data was used for these analyses

#Generally: The following will pose some questions to look at the population which this cohort is comprised of.
#Prominent and biologically also relevant features of the metadata will be looked at and visualised.
#Main Questions:
#Variant Found / not Found ?  => See Chapter 1.
#Age distribution             => Done
#Sex distribution             => Done
#How many Trio analyses etc?  => Future research
#Disease Categories?          => Done
#HP terms?                    => Done

# 2.2 Sex distribution of Cohort - Pie-Chart -----------------------------------------

# Disclaimer: Data scientists generally tend to dislike Pie-Charts.
# I don't fully care. I think this is one of the few instances where a Pie chart is valid.
# It has 3 or less categories and no "small slices", thus I believe it very much transports the information well enough.
# But I do see the point of Pie charts being generally suboptimal due to humans being far better at estimating lenghts than areas.

# An alternative where the length/Area problem is not as far exaggerated is the Donut-Plot, we could look into that more at a later time.
#Makes a factor out of `sex [male, female]` in the Logistics_and_Metadata dataframe, and then counts the elements.
  
LM_SEX <- Logistic_and_Metadata %>%
  count(factor(`sex [male, female]`))
  #Immediately rename the column, because That name is just confusing to read while coding
LM_SEX <- LM_SEX %>% 
  rename(sex = `factor(\`sex [male, female]\`)`)
#Immediately change it from a factor back into a character, because I'm better at working with those for now.
  LM_SEX$sex <- as.character(LM_SEX$sex)
#Add another column, which sums up the incomplete information sexes
LM_SEX <- LM_SEX %>%
  add_row(sex="missing sex", n=  sum(filter(LM_SEX, LM_SEX$sex %in% c("0",NA))$n ))
  LM_SEX <- filter(LM_SEX, !(LM_SEX$sex %in% c("0", NA, "AW")))
#Use the table to make a piechart

plot<-ggplot (LM_SEX, aes(x= "", y = n, fill= sex)) +
    scale_fill_manual(values =c("#CC79A7", "#56B4E9","#D55E00")) + #Uses Okabe-Ito colors
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start= 0) +
    geom_text(aes(x = 1.6, label = paste0(round(n/sum(n)*100), "%"), group = sex), position = position_stack(vjust = 0.5), color = "black") +
    geom_text(aes(x = 1.1, label = paste0(n), group = sex), position = position_stack(vjust = 0.5), color = "white") +
    labs(fill="sex",title = "sex distribution") +
    theme_void()+
    theme(plot.title = element_text(hjust = 0.5, size = 13))
    ggsave(filename = paste0(plot.directory, "Chapter_2_Sex.png"), plot = plot,width = 5, height = 4, dpi = 300)
    print(plot)
    rm (plot)
 

# 2.3 Ages of the Cohort --------------------------------------------------

#-------------------Age in years---------Column graph connected lines.
            
LM_Ages <- Logistic_and_Metadata %>%
  count(factor(`Age [years]`))%>%
  rename(Age.in.years= `factor(\`Age [years]\`)`)

#Data incompletely annotated in Excel also shows up as 0 in these counts
#Total counts are 10. But actual counts as manually determined with Excel are 4
#Set row for Age = 0 to actual counts 
#Mistakenly 0 annotated due to incomplete entry: 6

LM_Ages$n[LM_Ages$Age.in.years == 0] <- 4

#Plot
plot <-ggplot(LM_Ages, aes(x = as.numeric(as.character(Age.in.years)), y = n, group = 1)) +
    geom_bar(stat = "identity", fill = "grey") +
    labs(x = "age [years]", y = "number of patients", title = "age distribution") +
    scale_y_continuous(breaks = seq(0, max(LM_Ages$n)+5, by = 5), expand = expansion(mult = c(0, 0.1))) +
    theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5, size = 13),
        axis.title.y = element_text(size = 11),
        axis.title.x = element_text(size = 11))
    ggsave(filename = paste0(plot.directory, "Chapter_2_Age.png"), plot = plot,width = 5, height = 4, dpi = 300)
    print(plot)
    rm (plot)

# 2.4 Disease Categories --------------------------------------------------
#Disease categories were unified in nomenclature in chapter 0.2

#2.4.1 Make dataframe-------------------------------------------------
#Making dataframe with counts of disease categories & renaming the factor.
LM_disease_categories <- Logistic_and_Metadata %>%
  count(factor(`disease category`))%>%
  rename("disease categories"= `factor(\`disease category\`)`)
# Convert the disease categories to character
LM_disease_categories$`disease categories` <- as.character(LM_disease_categories$`disease categories`)
LM_disease_categories$`disease categories`[is.na(LM_disease_categories$`disease categories`)] <- "missing entry"
LM_disease_categories$`disease categories` <- factor(LM_disease_categories$`disease categories`, levels = LM_disease_categories$`disease categories`[order(-LM_disease_categories$n)])

#2.4.2 Make Plot-------------------------------------------------
plot <- ggplot (LM_disease_categories, aes(x = reorder(factor(`disease categories`), -n), y = n, fill = `disease categories`)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, reverse = TRUE)) +
  scale_fill_manual(values = palette.colors()) +
  geom_text(aes(label = n), position = position_dodge2(width = 0.9, reverse = TRUE), vjust=-0.5, color = "black") +
  labs(x = "disease categories", y = "number of cases", fill = "disease category", title="disease categories") +
  scale_y_continuous(breaks = seq(0, max(LM_disease_categories$n)+50, by=50), expand = expansion(mult = c(0, 0.1))) +
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, size = 13),
  axis.title.y = element_text(size = 11),
  axis.title.x = element_text(size = 11),
  axis.text.x = element_blank()
  #axis.text.x = element_text(angle = 20, hjust = 0.5, vjust = 0.5)
  )
ggsave(filename = paste0(plot.directory, "Chapter_2_disease_categories.png"), plot = plot,width = 5, height = 4, dpi = 300)
print(plot)
rm (plot)

# 2.5 HPO-Terms ----------------------------------------------------------------

# 2.5.1 Make dataframe ----------------------------------------------------------------------

#Currently the column "HP terms" is a string, which contains all the HPO terms of a case.
# Splitting the "HP terms" column into a vector of strings
# Could alternatively do this with the original Logistic_and_Metadata dataframe
LM_HPO <- Logistic_and_Metadata %>%
  mutate(HPO_terms = strsplit(`HPO terms`, "; "))%>%
# To not interfere with the later function, the empty columns will already be taken out here
LM_HPO <- filter(LM_HPO, HPO_terms != "NA")

# 2.5.2 Function HPO-term to description --------------------------------------------------
#The following code will write a function, which gets the HP-term (e.g.HP:0011297) and returns the description (e.g. "Abnormal digit morphology")
#It is necessary to download hp.obo for this to work.
#hp.obo is a list, which contains all the terms, IDs, descriptions and more of the Human Phenotypes.
#It is regularly updated and extended upon.
#The function will later be used to translate the IDs into more understandable phenotypes for plotting some graphs.

library(ontologyIndex)
hpo<-get_ontology(hpo_obo)

hpo_term_to_description<- function(hpo_ids){
  hpo_names<-c()
  for (hpo_id in hpo_ids){
    hpo_names<-c(hpo_names,
                 hpo$name[hpo$id==hpo_id])
  }
  return(hpo_names)
}

#Example codes
#hpo_term_to_description(c("HP:0011297","HP:0011297" ))
#parent_ids<-unlist(hpo$parents[hpo$id=="HP:0000118"] )
#hpo_term_to_description( parent_ids )


# 2.5.3 Function HP-Terms counting / Top 10 ----------------------------------------------

#Only works, as long as the top 10 entries are NOT including "NA".
#When no argument for x is given, will count all the HPO-terms across all disease-categories
#If a valid argument is given, will instead only look at the HPO-terms for the appropiate disease-category

Counting_HPO <- function(x = NULL) {
  title_plot <- paste0(x)
  if (is.null(x)) {
    df_Counting_HPO <- LM_HPO %>%
      unnest(HPO_terms) %>%
      count(HPO_terms) %>%
      arrange(desc(n))
  }
  #What happens if x is given as a character
  else if (is.character(x) == TRUE) {
    df_Counting_HPO <- LM_HPO[LM_HPO$`disease category` == paste0(x),] %>%
      unnest(HPO_terms) %>%	
      count(HPO_terms) %>%
      arrange(desc(n))  
  }

  #What happens if X is not a character
  #NOTE: I DO NOT YET HAVE ANY WAY TO CHECK, WHETHER THE Disease category actually exists.
  else {
    stop("Invalid argument. Please provide a valid disease category.")
  }
  
  #After the If/Else statements will draw a barplot with only the 10 most common HP-terms
  plot <- ggplot (df_Counting_HPO[1:10,], aes(x =reorder(hpo_term_to_description(df_Counting_HPO$HPO_terms[1:10]), n[1:10], decreasing = TRUE), y = n, fill = df_Counting_HPO$HPO_terms[1:10])) +
    geom_bar(stat = "identity", position = position_dodge2(width = 0.9, reverse = TRUE)) +
    #scale_fill_manual(values = ) +
    geom_text(aes(label = n), position = position_dodge2(width = 0.9, reverse = TRUE), vjust=-0.5, color = "black") +
    labs(x = "HP terms", y = "number of cases", fill = "HP IDs", title = paste0("Top 10 enriched HPO-terms in ",x,"clinical cases")) +
    scale_y_continuous(breaks = seq(0, max(df_Counting_HPO$n), by=20)) +
    scale_x_discrete(guide = guide_axis(n.dodge=3))+
    theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5, size = 13),
    axis.title.y = element_text(size = 11),
    axis.title.x = element_text(size = 11))
ggsave(filename = paste0(plot.directory, "Chapter_2_most_enriched_HPO-terms",title_plot,".png"), plot = plot,width = 8, height = 5, dpi = 300)   
print(plot)
rm(df_Counting_HPO)
}

Counting_HPO()  # <- total counts, doesn't work, if NA would be in the top 10 HPO terms
Counting_HPO("neurodevelopmental")  #<- counts all the neurodevelopmental casess (big majority)
Counting_HPO("organ abnormality")  #<- counts all the organ abnormality cases

# 3. QC ----------------------------------------------------------------------
# The QC was performed mainly with multi-QC, as denoted in the Labreport

# 4. Investigating optimisation of WES workflows ----------------------------------------------------------------------
# A subcohort of cases was reanalysed in context of an alignment against GrCH38 and the variants of these PEDIA-cases were identified and annotated with several programs as described in the Labreport (VEP, GATK etc.)
# The diagnostic workflow will employ genetic panel.

# 4.1 Importing Panel(s) ------------------------------------------------------

#Currently only the gene panel for intellectual disability is used.
panel_neurodevelopmental_df <- read_tsv(path_to_panel_neurodevelopmental, col_names = TRUE)
panels <- c(panel_neurodevelopmental_df) #When you want to include more panels, simply add them into this vector.
all_gene_symbols <- panels %>%
  lapply(function(panel) panels$`Gene Symbol`) %>% 
  unlist() %>% 
  unique()

# 4.2 Importing reassessed Data from TSV --------------------------

#For the course of this work an annotated, prefiltered tsv file was made after the realignment of the cases.
#This contains (nearly) ALL the variants, which were found in the part of the cohort, which was realigned.
#NOTE: NOT ALL CASES OF THE COHORT WERE REALIGNED. THE EXACT LIST OF THE ONES WHICH WERE REALIGNED CAN BE FOUND HERE:
#C:\Labrotation\snakemake_bam2fastQ
#C:\Labrotation\Archive_txt_files\Bam2fastQ

#The code uses a file path to the TSV, which was provided at the very top of the document.
# Import the TSV file with variants found in PEDIA cases after reassessment. It is imported as a dataframe
# Needs readr package, but that is contained within the tidyverse.
prefiltered_variant_df <- read_tsv(path_to_filtered_tsv, col_names = TRUE)
#NOTE, BUG: Sometimes the prefiltered_variant_df will weirdly enough have X instead of a % in the headers when imported like this. IDK why.
 
# 4.2 Make dataframe containing All info, if a variant was found -----------------------------------------------------
All_info_if_case_has_diagnostic_variant<-merge(Logistic_and_Metadata, Diagnostics_Variants_found, c="Position in Exom-Table")

# 4.3 Selecting Only reassessed PEDIA Cases for the comparison -----------------------------------------------------
#Only include data from the reassessed PEDIA-cases
#We can either manually use the lists mentioned above or automatically generate the list by the following command:
#levels(factor(prefiltered_variant_df$row.names)) with the first three digits of the level corresponding to the PEDIA-Case-ID.
#NOTE: the code MUST be fixxed, if 4 or 1 digit PEDIA-Cases are contained within. Currently it works for 2-3 digit long numbers (10-999)

#The following code first makes a list of all the PEDIA-Case-IDs. It takes the first three digits. 
List_PEDIA_Case_IDs_Reassessed <- substr(levels(factor(prefiltered_variant_df$sample)), start = 1, stop = 3)
#Now the underscores will be fixed.
List_PEDIA_Case_IDs_Reassessed <- gsub("_","", List_PEDIA_Case_IDs_Reassessed)
#Then converted into an numeric.
List_PEDIA_Case_IDs_Reassessed <- as.numeric(List_PEDIA_Case_IDs_Reassessed)
#Now make a new dataframe which only contains the Reassessed PEDIA-cases
All_info_if_case_has_diagnostic_variant_only_reassessed <- filter(All_info_if_case_has_diagnostic_variant, `PEDIA-Case-ID` %in% List_PEDIA_Case_IDs_Reassessed)
#As a neurodevelopmental panel will be used on the variants found in reassessment, all non-neurodevelopmental variants are filtered out
Reassessed_only_neurodevelopmental <- filter (All_info_if_case_has_diagnostic_variant_only_reassessed, `disease category` %in% "neurodevelopmental")


# 4.4 Conversion of GrCh37 Positions into GrCh38 --------------------------------------------------------------
#The diagnostic data uses GrCh37 and the reassesed data uses GrCh38 genomic positions.
#These must be converted into eachother, before a comparison is possible. 
#As I do not have ALL the files from the diagnositcs, we will just use it via text-format and the following website:
#https://genome.ucsc.edu/cgi-bin/hgLiftOver
#The output is given in a bed file.
#New columns were created in the Excel and the converted ones were input into it.

#NOTE: During the conversion one variant was lost, as it was deleted in the new annotation.
#chr5:981192331-981192331, another was not correctly con
#NOTE: After conversion not all Variants could be found in the joint call VCF file.

# 4.5 Comparison of the reassessed data with the diagnostic data -----------------------------------------------------

prefiltered_variant_df <- prefiltered_variant_df %>%
  mutate(`Comparison column` = paste(sub("chr", "", `%CHROM`), `%POS`, `%REF`, `%ALT`, sep = ":"))

All_info_if_case_has_diagnostic_variant <- All_info_if_case_has_diagnostic_variant %>%
  mutate(`Comparison column` = paste(Chromosome, `Position (GrCh38)`, Reference, Alternation, sep= ":"))
All_info_if_case_has_diagnostic_variant_only_reassessed <- All_info_if_case_has_diagnostic_variant_only_reassessed %>%
  filter(`Position (GrCh37)` != 0) %>%
  #Leaves out the rows, where no genomic/chromosomal position had been given. 
  #These are currently: two CAMK2B/CHD1 variants in PEDIA-Case 67 and 68 respectively.
  mutate(`Comparison column` = paste(Chromosome, `Position (GrCh38)`, Reference, Alternation, sep= ":"))



#4.5.1 Check whether all the diagnostic variants exist in the ginourmous TSV ----------------------------------------
#All_info_if_case_has_diagnostic_variant <- All_info_if_case_has_diagnostic_variant %>%
#  mutate(`Was found prefilter` = All_info_if_case_has_diagnostic_variant$`Comparison column` %in% prefiltered_variant_df$`Comparison column`)
All_info_if_case_has_diagnostic_variant_only_reassessed <- All_info_if_case_has_diagnostic_variant_only_reassessed %>%
  mutate(`Was found prefilter` = All_info_if_case_has_diagnostic_variant_only_reassessed$`Comparison column` %in% prefiltered_variant_df$`Comparison column`)

#All_info_if_case_has_diagnostic_variant_NOT_found_only_reassessed <- filter(All_info_if_case_has_diagnostic_variant_only_reassessed, `Was found prefilter` == FALSE)
#23 of 104 variants were not found in the prefiltered_variant_df

# OBSOLETE: 4.6 Filtering Automation ----------------------------------------

#The following function will be thoroughly annotated. It will be seperated into several steps.
#Currently the plan is to annotate every step. Afterwards the example code for that step is given, but not executed.
#All the steps are executed one after the other at the end.

#Step 1: Filtering according to the cutoff-quality
#The cutoff is used, to filter out any and all rows of the tsv, which have lesser quality than was given.
#NOTE: during the snakemake realignment automatically anythign below 30 WAS NOT WRITTEN INTO THE TSV.

#Step 1: Example code
#dataframe_prefilter %>% 
#  filter(`%QUAL` > cutoff_quality)

#Step 2: Filtering according to EnsemblID in the panel(s) given.
#Combine all EnsemblIDs from the panels into a single vector. This actually works, which is quite surprising. 
#The dataframes are converted into a list in this step.
#And they also exist for pseudogenes etc
#This enables us to filter out more and more accurately

#Step 2: Example code
#dataframe_prefilter %>% 
#filter (Gene  %in% all_EnsemblId_list)

#Step 3: Look at a certain class of impact as predicted by VEP
#Look at only the High impact genes
#Impact <- "HIGH"

#Step 3: Example Code
#dataframe_prefilter %>% 
#filter (IMPACT == paste0(Impact))
#Current possibilites: (levels(as.factor(dataframe_prefilter$IMPACT)) )"HIGH"     "LOW"      "MODERATE" "MODIFIER""

#Step 6: As an estimation instead of Step 5 focus on the heterozygous mutations => Filter 
# Only take => "0/1" "0|1" in column `#` of prefiltered_variant_df

#Step 6: Example Code
#dataframe_prefilter %>%
#filter (`#` %in% c("0/1", "0|1")) 

#Step 7: Filter for clinical significance.
#This must be done in multiple steps. 
#First every entry, which contains "pathogenic" under clinical significance will be kept.
#Next every entry, which contains "benign" will not be kept

#Example code
#dataframe_prefilter %>%
#filter(grepl("pathogenic", CLIN_SIG)) %>%
#filter(!grepl("benign", CLIN_SIG))


#OBSOLETE: 4.6.1 Check whether all the diagnostic variants exist in the ginourmous TSV ----------------------------------------
#Function. NOTE this function is currently obsolete, as another approach was taken instead.
Filtering_automation <- function(dataframe_prefilter = prefiltered_variant_df,dataframe_diagnostics = All_info_if_case_has_diagnostic_variant_only_reassessed, cutoff_quality=30, panels = c(panel_neurodevelopmental_df), Impact = "HIGH", zygosity = "heterozygous", clinical_significance = "pathogenic/-benign") {
  all_EnsemblId_list <- panels$`EnsemblId(GRch38)`
  #Quality Cutoff
  postfiltered_variant_df <- dataframe_prefilter %>% 
    filter(`%QUAL` > cutoff_quality)

  dataframe_diagnostics <- dataframe_diagnostics %>%
    mutate(`Found after Quality cutoff` = dataframe_diagnostics$`Comparison column` %in% postfiltered_variant_df$`Comparison column`)

  #OBSOLETE filter(SYMBOL %in% all_gene_symbols_list)

  #Panel-filter
  postfiltered_variant_df <- postfiltered_variant_df %>%  
  filter (Gene  %in% all_EnsemblId_list)
  dataframe_diagnostics <- dataframe_diagnostics %>%
  mutate(`Found after Panel-filter` = dataframe_diagnostics$`Comparison column` %in% postfiltered_variant_df$`Comparison column`)
      
  #Impact filter
  postfiltered_variant_df <- postfiltered_variant_df %>%  
  filter (IMPACT == paste0(Impact))
  dataframe_diagnostics <- dataframe_diagnostics %>%
  mutate(`Found after Impact-filter` = dataframe_diagnostics$`Comparison column` %in% postfiltered_variant_df$`Comparison column`)  
  
  #Zygosity-filter, if given, currently only heterozygous possible.
  #if (zygosity == "heterozygous") {
  #  postfiltered_variant_df <- postfiltered_variant_df %>%
  #    filter ((`#` %in% c("0/1", "0|1")))
  #dataframe_diagnostics <- dataframe_diagnostics %>%
  #mutate(`Found after zygosity-filter` = dataframe_diagnostics$`Comparison column` %in% postfiltered_variant_df$`Comparison column`)
  #}
  
  
  #Filters for clinical significance, currently only works for "pathogenic/-benign"
  #"Pathogenic/-benign"
  if (clinical_significance == "pathogenic/-benign"){
  postfiltered_variant_df <- postfiltered_variant_df %>%
    filter(grepl("pathogenic", CLIN_SIG)) %>%
    filter(!grepl("benign", CLIN_SIG))
  view(postfiltered_variant_df)
  }
  dataframe_diagnostics <- dataframe_diagnostics %>%
  mutate(`Found after clinical significance filter` = dataframe_diagnostics$`Comparison column` %in% postfiltered_variant_df$`Comparison column`)
  
  
  #Put the postfiltered_variant_df into the permanent environment
  postfiltered_variant_df <<- postfiltered_variant_df
  dataframe_diagnostics <<- dataframe_diagnostics
  
}

#Run filtering with default settings
Filtering_automation()

# Checking and comparing how many of the variants can still be found

All_info_if_case_has_diagnostic_variant <- All_info_if_case_has_diagnostic_variant %>%
  mutate(`Was found postfilter` = All_info_if_case_has_diagnostic_variant$`Comparison column` %in% postfiltered_variant_df$`Comparison column`)

dataframe_diagnostics <- dataframe_diagnostics %>%
  mutate(`Was found postfilter` = dataframe_diagnostics$`Comparison column` %in% postfiltered_variant_df$`Comparison column`)

sum(dataframe_diagnostics$`Was found postfilter`)
nrow(dataframe_diagnostics)

# 4.7 Optimising WES filters for finding clinically relevant variants -----------------------------------------------------
#The following code will use several filters for analysing variants.
#It is highly unoptimised and leaves room for efficient computational improvements
#ClinSig according to Clinvar will be used to determine the pathogenicity of a variant.
#Several cutoffs for the gnomAD Frequencies will be used.
#Several filters for the VEP-predicted impact will be used.
#An in-house gene panel used for the assessment of Neurodevelopmental cases will be used.
#The total number of variants will be counted and the number of pathogenic relevant variants will be counted and plotted.
ClinSig_filter <- c("likely_pathogenic","likely_pathogenic&pathogenic", "likely_pathogenic&pathogenic/likely_pathogenic&pathogenic", "likely_pathogenic&uncertain_significance", "not_provided&likely_pathogenic&pathogenic", "pathogenic", "pathogenic&likely_pathogenic", "pathogenic&likely_pathogenic", "pathogenic&pathogenic/likely_pathogenic", "pathogenic/likely_pathogenic", "pathogenic/likely_pathogenic&likely_pathogenic", "pathogenic/likely_pathogenic&pathogenic" )
all_EnsemblId_list_static <- panel_neurodevelopmental_df$`EnsemblId(GRch38)`

#Making the dataframe for plotting-----------------------------------------------------
#Gage how many pathogenic variants are found in the cohort for the labreport.
#nrow(prefiltered_variant_df %>% filter(CLIN_SIG %in% ClinSig_filter))


#AF < 0.01%
optimus_df_AF_lenient_all <- prefiltered_variant_df %>%
  filter (gnomADe_AF < 0.0001) #= 0.01%)

#AF < 0.005%
optimus_df_AF_moderate_all <- prefiltered_variant_df %>%
  filter (gnomADe_AF < 0.00005)  #= 0.05%

#AF < 0.0001%
optimus_df_AF_strict_all <- prefiltered_variant_df %>%
  filter (gnomADe_AF < 0.00001) #= 0.001%

#Impacts: HIGH and MODERATE
optimus_df_Impact_lenient_all <- prefiltered_variant_df %>%
  filter (IMPACT %in% c("HIGH", "MODERATE"))

#Impact: HIGH
optimus_df_Impact_strict_all <- prefiltered_variant_df %>%
  filter (IMPACT %in% c("HIGH"))

#Panel filter
optimus_df_panel_all <- prefiltered_variant_df %>%
  filter (Gene %in% all_EnsemblId_list_static)

#Combination lenient
optimus_df_AF_Impact_lenient_all <- prefiltered_variant_df %>%
  filter (gnomADe_AF < 0.0001) %>% #= 0.01%
  filter (IMPACT %in% c("HIGH", "MODERATE"))

# Combination strict
optimus_df_AF_Impact_pan_strict_all <- prefiltered_variant_df %>%
  filter(gnomADe_AF < 0.00001) %>% # = 0.001%
  filter(IMPACT %in% c("HIGH")) %>%
  filter(Gene %in% all_EnsemblId_list_static)

#Make dataframe for plotting
WES_optimus_filtering_df <- data.frame(
  `all or pathogenic` = c(rep(c("variants post filter", "pathogenic variants"),8)),
  `used filters` = c(rep(("AF < 0.1 %"),2),rep(("AF < 0.05 %"),2),rep(("AF < 0.001 %"),2),rep(("Impact moderate"),2),rep(("Impact high"),2),rep(("ND Panel"),2),rep(("Combination lenient"),2),rep(("Combination strict"),2)),
  `number of variants` = c(nrow(optimus_df_AF_lenient_all), nrow(optimus_df_AF_lenient_all %>% filter(CLIN_SIG %in% ClinSig_filter)), nrow(optimus_df_AF_moderate_all), nrow(optimus_df_AF_moderate_all %>% filter(CLIN_SIG %in% ClinSig_filter)), nrow(optimus_df_AF_strict_all), nrow(optimus_df_AF_strict_all %>% filter(CLIN_SIG %in% ClinSig_filter)), nrow(optimus_df_Impact_lenient_all), nrow(optimus_df_Impact_lenient_all %>% filter(CLIN_SIG %in% ClinSig_filter)), nrow(optimus_df_Impact_strict_all), nrow(optimus_df_Impact_strict_all %>% filter(CLIN_SIG %in% ClinSig_filter)), nrow(optimus_df_panel_all), nrow(optimus_df_panel_all %>% filter(CLIN_SIG %in% ClinSig_filter)), nrow(optimus_df_AF_Impact_lenient_all), nrow(optimus_df_AF_Impact_lenient_all %>% filter(CLIN_SIG %in% ClinSig_filter)), nrow(optimus_df_AF_Impact_pan_strict_all), nrow(optimus_df_AF_Impact_pan_strict_all %>% filter(CLIN_SIG %in% ClinSig_filter))
  )
)

#remove the smaller dataframes
rm(optimus_df_AF_lenient_all, optimus_df_AF_moderate_all, optimus_df_AF_strict_all, optimus_df_Impact_lenient_all, optimus_df_Impact_strict_all, optimus_df_panel_all, optimus_df_AF_Impact_lenient_all, optimus_df_AF_Impact_pan_strict_all) 



#4.7.1 Plotting-----------------------------------------------------

#It was opted to use a table instead of plotting the data.



