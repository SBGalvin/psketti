
# pskettify example -------------------------------------------------------

library(eRm)
library(psketti)

data("FakeData") # load data
# restructure fake data
Fake_Data_scores <- reshape(FakeData[, c("ID", "Item", "X")],
                           timevar = "Item",
                           idvar = "ID",
                           direction = "wide")
# for eRm new names
names(Fake_Data_scores) <- c("ID",
                             paste0("i",
                                    sprintf(fmt  = "%02d", 1:23)))

row.names(Fake_Data_scores) <- Fake_Data_scores$ID
Fake_Data_scores$ID <- NULL

fake_rm   <- RM(Fake_Data_scores) # Estimate Rasch model

psk_data  <- pskettify(fake_rm)   # pskettify data


# psketto example ---------------------------------------------------------

# plot IRF in default colours
psk_1_present <- psketto(psk_data,
                         style = "present",
                         item = "i01",
                         item.label = "i01")
psk_1_present # plot output

# plot IRF in default greyscale colours
psk_1_print <- psketto(psk_data,
                       style = "print",
                       item = "i01",
                       item.label = "i01")
psk_1_print # plot output


# psketti example ---------------------------------------------------------

multi_plot <- psketti(pskettified_data = psk_data)
multi_plot # plot call instructions

multi_plot$Plot.List[['i01']][[1]]



# tabliatellify example ---------------------------------------------------

# Prepare response options factor
r_o <- factor(sort(unique(FakeData$K)),          # input var
              levels = sort(unique(FakeData$K)), # factor levels
              ordered = TRUE)                    # ordered
# tabliatellify
tlt_data <- tabliatelle(x = FakeData, eRm.obj = fake_rm,
                        ID = "ID", Item = "Item", K = "K",
                        response_options = r_o)

tlt_data # output


# psketti_distractor example ----------------------------------------------

# multiple plots
spag_plot <- psketti_distractor(ID = "ID",              # set ID column
                                Item = "Item",          # set Item column
                                K= "K",                 # Set resp categories 
                                x = FakeData,           # select data
                                eRm.obj = fake_rm,      # select eRm object
                                response_options = r_o, # set resp options
                                p.style = "present")    # set plotting style
 
spag_plot                         # plot call instructions
spag_plot$Plot.List[['i01']][[1]] # plot item 1
 

# item_fit_table example --------------------------------------------------
itemFit_psk <- item_fit_table(fake_rm)
itemFit_psk

# psketti_msq example -----------------------------------------------------
MSQplot <- psketti_msq(itemFit_psk)



# ingrediente example -----------------------------------------------------
# Example 1
# For dichotomous Rasch model
library(psketti)
data("FakeData")

K_opt <- factor(LETTERS[1:5], levels = LETTERS[1:5], ordered = TRUE)
score_report <- ingrediente(x = FakeData,
                            Item = "Item",
                            ID = "ID",
                            Score = "X",
                            K = "K",
                            K_options = K_opt,
                            Index = "Index")

# show score report for values with a total score <= 5
score_report[score_report$total_score <= 1, ]
 # Score report ordering response string by item difficulty
 data("FakeItems")
 FI2 <- FakeItems[order(FakeItems$Beta),]
 row.names(FI2)<- NULL
 FI_factor <- factor(FI2$Item, levels = FI2$Item, ordered = TRUE)



 score_report2 <- ingrediente(x = FakeData,
                              Item = "Item",
                              ID = "ID",
                              Score = "X",
                              K = "K",
                              K_options = K_opt,
                              Index = FI_factor)

# show score report for values with a total score <= 5
score_report2[score_report2$total_score == 21, ]


# Example 2
# For Rasch partial credit model
library(dplyr)
library(tidyr)
data("FakePCMData")
data("FakePCMItems")

# Arrange Data, wide to long
fpcm <- FakePCMData %>% 
  pivot_longer(cols = -ID, values_to = "Response", names_to = "Item") %>% 
  mutate(X = Response) %>% 
  mutate(K = as.character(Response)) %>% 
  mutate(K = recode(K, "0" = "A", "1" = "B", "2" = "C", "3" = "D"))
class(fpcm)
# factor variable: Index for item order
F2            <- FakePCMItems[, c("Item", "Beta")] # extract relevant cols
F2            <- F2[order(F2$Beta),]               # order dataframe
row.names(F2) <-  NULL                             # drop rownames     

# create factor variable
F_factor <- factor(F2$Item,
                   levels = F2$Item,
                   ordered = TRUE)
#apply factor to data frame
fpcm$Index <- fpcm$Item                         # Item -> Index
fpcm$Index <- factor(fpcm$Index,
                     levels = levels(F_factor),
                     ordered = TRUE)

fpcm      <- as.data.frame(fpcm) # ensure this is a dataframe!!

# factor variable for K categories
K_opt     <- factor(LETTERS[1:4],
                    levels = LETTERS[1:4],
                    ordered = TRUE)
# produce score report
score_pcm <- ingrediente(x = fpcm,
                         Item = "Item",
                         ID = "ID",
                         Score = "X",
                         K = "K",
                         Index = "Index",
                         K_options = K_opt)

score_pcm[score_pcm$total_score < 2, ] # print out score report
