b10k<-read_xlsx("../data/b10k.xlsx")
model.traits.b10k <- b10k


table(b10k$b10k.habitat)
index3 <- model.traits.b10k$b10k.habitat != 3
model.traits.b10k$binary_habitat <- model.traits.b10k$b10k.habitat
model.traits.b10k$binary_habitat[index3] = 1


table(b10k$b10k.unidirectional)
index4 <- model.traits.b10k$b10k.unidirectional != 0
model.traits.b10k$binary_unidirectional <- model.traits.b10k$b10k.unidirectional
model.traits.b10k$binary_unidirectional[index4] = 3


table(b10k$territory)
index2 <- model.traits.b10k$territory != 3
model.traits.b10k$binary_territory <- model.traits.b10k$territory
model.traits.b10k$binary_territory[index2] = 1

table(b10k$s_bond_stab)
index1 <- model.traits.b10k$s_bond_stab != 3
model.traits.b10k$binary_s_bond_stab <- model.traits.b10k$s_bond_stab
model.traits.b10k$binary_s_bond_stab[index1] = 1


table(b10k$b10k.migration)
index5 <- model.traits.b10k$b10k.migration != 1
model.traits.b10k$binary_migration <- model.traits.b10k$b10k.migration
model.traits.b10k$binary_migration[index5] = 3


# log transform mass and bill traits 
model.traits.b10k$b10k.mass <- log(model.traits.b10k$b10k.mass)
hist(model.traits.b10k$b10k.mass)
model.traits.b10k$b10k.bill_nares <- log(model.traits.b10k$b10k.bill_nares)
hist(model.traits.b10k$b10k.bill_nares)


write.csv(model.traits.b10k,"../data/b10kb.csv")

