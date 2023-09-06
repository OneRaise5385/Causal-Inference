#t检验
score = c(85,92,81,95,76,80)
score_t_test <- t.test(score~c(1,1,0,1,0,0))
write.table(score_t_test,file = '')

# 欧氏距离
iris[1:6,1:4]
dist(iris[1:6,1:4],method = 'euclidean')

# 马氏距离
library(StatMatch)
mahalanobis.dist(iris[1:6,1:4])

# 匹配分析
library(MatchIt)
library(foreign)
library(cobalt)
library(caret)
library(zelig)
data(lalonde)
head(lalonde)
# 在R语言中，attach函数的作用是将一个数据框或列表中的变量添加到搜索路径中，
# 使得这些变量在代码中可以直接使用，而不需要使用$符号来引用。
attach(lalonde)

# 精确匹配
out1_exact <- matchit(treat ~ age + educ + race + married + nodegree,
                     data = lalonde, method = 'exact')
# 细分
out2_subc <- matchit(treat ~ re74 + re75 + educ + age,
                   data = lalonde, method = 'subclass',subclass = 5)
# 最近距离匹配
out3_nearest <- matchit(treat ~ re74 + re75 + educ + age,
                   data = lalonde, method = 'nearest')
# 1：2最优匹配
out4_optimal <- matchit(treat ~ re74 + re75 + educ + age,
                       data = lalonde, method = 'optimal',ratio = 2)
# 全匹配
out5_full <- matchit(treat ~ re74 + re75 + educ + age,
                       data = lalonde, method = 'full')
# 遗传匹配
out6_genetic <- matchit(treat ~ re74 + re75 + educ + age,
                    data = lalonde, method = 'genetic')
# 输出结果
summary(out1_exact)
summary(out2_subc)
summary(out3_nearest)
summary(out4_optimal)
summary(out5_full)
summary(out6_genetic)

# 检验平衡性
love.plot(out6_genetic)
plot(out6_genetic,type = 'jitter')
plot(out6_genetic,type = 'hist')

# 基于匹配样本进行回归分析
ate <- zelig()


# 双重稳健估计
library(CausalGAM)
library(MatchIt)
library(foreign)
head(lalonde)
attach(lalonde)

ATE.out <- estimate.ATE(pscore.formula = treat~re74 + re75 + educ + race + age,
                        pscore.family = binomial,
                        outcome.formula.t = re78~treat + re74 + re75 + educ)










