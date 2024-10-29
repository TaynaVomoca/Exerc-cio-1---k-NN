#4
#Tabela de frequencia para os rótulos
table(Exercício_1_Qualidade_do_leite$Grade)

#5
#Codifificando a variável como fator
Exercício_1_Qualidade_do_leite$Grade <- factor(Exercício_1_Qualidade_do_leite$Grade)

#6
#Criando uma funçao que normaliza
normaliza <- function(x){return((x - min(x))/(max(x) - min(x)))}
#Normalizando os dados numéricos
Exercício_1_Qualidade_do_leite_n <- as.data.frame(lapply(Exercício_1_Qualidade_do_leite[1:7], normaliza))

#7
#Criando datasets de treino e teste
#80% para treino e 20% para teste

Exercício_1_Qualidade_do_leite_treino <- Exercício_1_Qualidade_do_leite_n[1:848,]
Exercício_1_Qualidade_do_leite_teste <- Exercício_1_Qualidade_do_leite_n[849:1059,]


#8

#Separando rótulos
Exercício_1_Qualidade_do_leite_treino_rotulos <- Exercício_1_Qualidade_do_leite[1:848,]$Grade
Exercício_1_Qualidade_do_leite_teste_rotulos <- Exercício_1_Qualidade_do_leite[849:1059,]$Grade

install.packages("class")
install.packages("caret")
library(class)
library(caret)

predicoes <- knn(train = Exercício_1_Qualidade_do_leite_treino, 
                 test = Exercício_1_Qualidade_do_leite_teste, 
                 cl = Exercício_1_Qualidade_do_leite_treino_rotulos, 
                 k = k)

confusionMatrix( Exercício_1_Qualidade_do_leite_teste_rotulos, predicoes)

##########################################
#Escolhendo o k
##########################################

acuracia <- c() #serve para criar vetor

for(k in 1:30){
  
  set.seed(1234)
  
  predicoes <- knn(train = Exercício_1_Qualidade_do_leite_treino, test = Exercício_1_Qualidade_do_leite_teste, cl = Exercício_1_Qualidade_do_leite_treino_rotulos, k = k)
  
  matriz <- confusionMatrix(Exercício_1_Qualidade_do_leite_teste_rotulos, predicoes)
  
  acuracia <- c(acuracia, matriz[['overall']]['Accuracy'])
}

acuracia

# 8b) Exibindo graficamente o erro em função do valor de k
plot(1:30, 1 - acuracia, type = "b", col = "blue", xlab = "Valor de K", ylab = "Erro de Classificação")

# 8c) Encontrando o melhor valor de k
melhor_k <- which.max(acuracia)
cat("Melhor valor de k:", melhor_k, "\n")

#9)
set.seed(1234)
predicoes_final <- knn(train = Exercício_1_Qualidade_do_leite_treino, 
                       test = Exercício_1_Qualidade_do_leite_teste, 
                       cl = Exercício_1_Qualidade_do_leite_treino_rotulos, 
                       k = melhor_k)

# 9a) Construindo a matriz de confusão
matriz_final <- confusionMatrix(Exercício_1_Qualidade_do_leite_teste_rotulos, predicoes_final)
cat("Matriz de confusão:\n")
print(matriz_final)

# 9b) Analisando o desempenho
cat("Acurácia final:", matriz_final[['overall']]['Accuracy'], "\n")

