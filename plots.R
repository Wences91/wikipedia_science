library(ggplot2)
library(dplyr)

df_cat <- read.delim2('results/page_category.tsv', stringsAsFactors=FALSE)

plot_1 <- ggplot(df_cat) +
  geom_boxplot(aes(x='', y=categories), outlier.alpha=0.5, color='black') +
  geom_point(aes(x='', y=mean(categories)), shape=4, color='black') +
  coord_flip() +
  labs(x='', y='Categories per page') +
  theme_classic() +
  theme(axis.text = element_text(size=10, color='black'))

png('figures/page_cat.png', res=350, width=2000, height=700)
plot_1
dev.off()

df_cat_freq <- as.data.frame(table(df_cat$categories),
                             stringsAsFactors=FALSE)

df_cat_freq$Var1 <- as.integer(df_cat_freq$Var1)

df_cat_freq <- df_cat_freq[order(df_cat_freq$Var1, decreasing=FALSE),]

png('figures/page_cat_den.png', res=350, width=2000, height=1200)
ggplot(df_cat_freq) +
  geom_area(aes(x=Var1, y=Freq), fill='black') +
  labs(x='', y='') +
  theme_classic() +
  theme(axis.text = element_text(size=10, color='black'))
dev.off()

df_page <- read.delim2('results/category_page.tsv', stringsAsFactors=FALSE)

categories <- read.delim2('data/category.tsv', stringsAsFactors=FALSE)

top_categories <- df_page[order(df_page$pages, decreasing=TRUE)[1:20],]
top_categories <- inner_join(top_categories, categories[,c('category_id', 'title')],
                             by='category_id')
top_categories$title <- gsub('_', ' ', top_categories$title)

ggplot(top_categories) +
  geom_col(aes(y=reorder(title, pages), x=pages), fill='black') +
  labs(x='Pages', y='Category') +
  theme_classic() +
  labs(y='') +
  theme(axis.text = element_text(size=10, color='black'))
