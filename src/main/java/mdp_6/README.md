策略梯度的第一个优势是其能够处理连续场景。价值函数近似就不适用了连续的强化学习场景。
因为 A 是一个无限集合的情况下，我们无法计算 argmaxa∈Aq(s,a) 了。
但如果我们使用的是策略梯度，π(s) 输出实数值。
当然这一部分可以通过改进价值函数形式的方式解决。 
