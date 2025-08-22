# Questão 5.20
# Eq. estimada:
#                 \hat{y} = 26 + 4x_1 + 3x_2 + 24z + 2zx_1 + 2zx_2 (Livro)
#                 \hat{y} = 26 + 4x_1 + 3x_2 + 24z - 2zx_1 + 2zx_2 (Corrigida)

# a) \hat{y} = 26 + 4x_1 + 3x_2 p/ i = 1, . . ., 7

# b) \hat{y} = 50 + 2x_1 + 5x_2 p/ i = 8, . . ., 13

# c) H_0: gama = deta1 = delta2 = 0       teste se a mudança estrutural foi 
#                                       significativa

# Retirar a variável Z para testa a hipoótese:
# \hat{y} = 9.6923 + 6x_1 + 4.3846x_2     (modelo 2)

# F = SQR_modelo_2 - SQR_modelo_original / numero de variaveis retiradas
#     ------------------------------------------------------------------
#               384 / GL residuo do modelo original
#   = [(1069 - 384) / 3] / [384 / 7]
#   = 4.17

qf(0.95, 3, 7)
