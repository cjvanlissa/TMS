df <- googlesheets4::read_sheet('https://docs.google.com/spreadsheets/d/1gu6VY_INm8sNfA3sBweHvqlioce7ocOosZbwDq6BN34/edit?usp=sharing')
df <- df[!df$`Presenting author` == "Dominik Gallenberger", ]
saveRDS(df, "df.RData")
df <- readRDS("df.RData")
names(df) <- sapply(strsplit(names(df), " "), `[[`, 1)
table(df$Presentation)
cor(df[, 10:11])
df$rating <- rowMeans(df[, 10:11])
df <- df[!is.na(df$rating), ]
df <- df[-which(df$Notified == 1), ]
# df <- df[order(df$rating, decreasing = TRUE), ]

df$type <- df$Presentation
df$type[df$rating <= 6.5 & df$type == "Full talk"] <- "Lightning talk"

table(df$type, df$Presentation)
#Full talk 8
#Lightning talk 16
(5*60) - sum(c("Lightning talk" = 7, "Full talk" = 30)[df$type], na.rm = TRUE)

# df1 <- read.csv("submissions.csv", stringsAsFactors = FALSE)
# df <- rbind(df1, df)
# write.csv(df, "submissions.csv", row.names = FALSE)

library(gmailr)
gm_auth_configure(path = "/home/cjvanlissa/Nextcloud/client_secret_229801455603-ev9m00bd3beqdqp2dan3c43dnm4q81ut.apps.googleusercontent.com.json")
gm_auth()
# Email students ----------------------------------------------------------
# i = sample.int(nrow(df_comb), 1)
for(i in 1:nrow(df)){
  thisrow <- df[i, ]
  if(is.na(thisrow$Email)) next
  bd <- glue::glue('Dear {thisrow["Presenting"]},

Thank you for submitting your proposed {thisrow["Presentation"]} titled "{thisrow["Title"]}" for the inaugural Theory Methods Conference in Tilburg, September 30th - October 2nd!

Congratulations: two reviewers have rated your presentation, and recommended you be included in the program!

{c(" ", "Since we received many more excellent submissions than slots for full presentations, we would like to ask you to prepare a **lightning talk** instead.")[(thisrow["type"] != thisrow["Presentation"]) + 1L]}

Early registration is available here:  https://edu.nl/guntn

After you fill out the form, Tilburg University will contact you for payment and provide you with a receipt.

Thanks to an in-kind donation from keynote speaker prof. dr. Andreas Gloeckner, we can offer fee waivers (including the conference dinner) for early-career scholars who do not have university funding to attend. You can apply for the fee waiver, and we will award these based on your need and our budget.

We look forward to receiving you at this vibrant and inspiring event on the green campus of Tilburg University!

Sincerely,
the TMS board

Caspar van Lissa
Noah van Dongen
Jason Nak
Luiza Yuan
Ward Eiling')

  my_email_message <- gm_mime() |>
    gm_to(thisrow[1,"Email"]) |>
    gm_from("c.j.vanlissa@gmail.com") |>
    gm_subject("Decision Notification Theory Methods Conference") |>
    gm_text_body(bd)

  ## send it
  gm_send_message(my_email_message)
  Sys.sleep(2)

}


library(theorytools)
library(umap)
# df <- read.csv("submissions.csv", stringsAsFactors = FALSE)

emb <- theorytools::get_embeddings(df$Title, model_path = "/home/cjvanlissa/git_repositories/sentence_transformers")
saveRDS(emb, "emb.RData")
emb <- readRDS("emb.RData")

emb_umap <- umap::umap(emb, n_components = 10)
mat_dist <- dist(emb_umap$layout)
clust <- hclust(mat_dist)
plot(clust)
cut <- cutree(clust, k = 5)
m <- as.matrix(mat_dist)
plot(emb_umap$layout[, 1], emb_umap$layout[, 2])
df$cluster <- cut
df$duration[df$type == "Lightning talk"] <- 5
df$duration[df$type == "Full talk"] <- 30

library(openxlsx)
openxlsx::write.xlsx(df, "clusters.xlsx")
