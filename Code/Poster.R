## Poster Image

library(swaRmverse)
scores <- read.csv('../Data/Metrics/metrics_pigeons.csv')
scores <- new_pca$swarm_space
install.packages("extrafont")
library(extrafont)
extrafont::font_import()
extrafont::loadfonts()

X <- ggplot2::ggplot(scores, ggplot2::aes(x = PC1, y = PC2, fill = species,
                                         shape = species)) +
  ggplot2::geom_point( color = 'black', size = 5, stroke = 1)+
  ggplot2::labs(fill = 'Species:', shape = 'Species:', y = 'PC2 - Plasticity',
                x= 'PC1 - Group cohesion & order',
                title = 'PCA space')+
  ggplot2::scale_fill_manual(
    values = c( "red", "yellow", "green",  "blue", "orange","pink","purple" ),
    #breaks = c('Pigeon (Gagliardo)','Pigeon (Santos)', 'Egyptian Fruit Bat',  'Greater Spear Nosed Bat' )
    #breaks = c('fish', 'pigeons', 'goats', 'baboons', 'pigeonoids')
  )+
  ggplot2::scale_shape_manual(values =c( 21,22,21,22, 23, 24,25),
                              # breaks = c('Sticklebacks','Pigeons', 'Goats',  'Baboons' )
                              #breaks = c('Pigeon (Gagliardo)','Pigeon (Santos)', 'Egyptian Fruit Bat',  'Greater Spear-Nosed Bat')
  )+
  ggplot2::scale_size_manual(values=c(4, 5.5, 7))+
  ggplot2::theme_bw() +
  ggplot2::coord_equal()+
  ggplot2::theme(legend.position = 'right',
                 plot.background = ggplot2::element_rect(fill = 'transparent'),
                 legend.background = ggplot2::element_rect(fill = 'transparent'),
                 legend.box.background = ggplot2::element_rect(fill = 'transparent'),
                 panel.grid = ggplot2::element_blank(),
                 panel.background = ggplot2::element_rect(fill = 'transparent'),
                 plot.title = ggplot2::element_text( size = 20, family = 'Palatino Linotype', hjust = 0.5),
                 axis.title =  ggplot2::element_text(color = 'black', size = 20, family = 'Palatino Linotype'),
                 axis.text =   ggplot2::element_blank(),
                 legend.title = ggplot2::element_text( size = 18, family = 'Palatino Linotype'),
                 legend.text = ggplot2::element_text( size = 18, family = 'Palatino Linotype') )+
  ggplot2::guides(size = 'none', fill = ggplot2::guide_legend(override.aes = list(size = 4)))+
  scale_x_continuous(limits = c(-3,3))+
  scale_y_continuous(limits = c(-2,2)
  )

X

ggplot2::ggsave(filename = '../output/pca4posterpres3.png', plot = X, width = 13, height = 13)
