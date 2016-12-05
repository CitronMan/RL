模型无关的策略学习，是在不知道马尔科夫决策过程的情况下学习到最优策略。
模型无关的策略学习主要有三种算法: MC Control, SARSA 和 Q learning
 在模型相关强化学习中，我们的工作是找到最优策略的状态价值Rendered by QuickLaTeX.com。但是在模型无关的环境下，这个做法却行不通。如果我们在模型无关环境下找最优策略的状态价值Rendered by QuickLaTeX.com，
 在预测时，对状态 Rendered by QuickLaTeX.com 最优策略如下所示

 
 q(s,a)是用来选择策略的一个函数。q(s,a)的值越大，说明这个策略越好，即针对这个state的action越好。
 而且这个衡量标准是考虑了未来的。
   Q Learning 的收敛性是很好玩的。Q Learning 与 MC Control 和 SARSA 一样采用了 ϵ-贪婪策略，但 Q Learning 的状态-动作价值却能收敛到最优策略的状态-动作价值
 