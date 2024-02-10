import torch

x1 = torch.Tensor([2]).double()
x1.requires_grad = True
w1 = torch.Tensor([-3]).double()
w1.requires_grad = True
x2 = torch.Tensor([0]).double()
x2.requires_grad = True
w2 = torch.Tensor([1]).double()
w2.requires_grad = True
b = torch.Tensor([6.8813735870195432]).double()
b.requires_grad = True
x1w1 = x1 * w1
x1w1.retain_grad()
x2w2 = x2 * w2
x2w2.retain_grad()
x1w1x2w2 = x1w1 + x2w2
x1w1x2w2.retain_grad()
x1w1x2w2b = x1w1x2w2 + b
x1w1x2w2b.retain_grad()
coshx1w1x2w2b = x1w1x2w2b.cosh()
coshx1w1x2w2b.retain_grad()
sinhx1w1x2w2b = x1w1x2w2b.sinh()
sinhx1w1x2w2b.retain_grad()
tanhx1w1x2w2b = x1w1x2w2b.tanh()  # sinhx1w1x2w2b / coshx1w1x2w2b
tanhx1w1x2w2b.retain_grad()

tanhx1w1x2w2b.backward()

print(tanhx1w1x2w2b.data.item())
print("x1", x1.grad.data)
print("w1", w1.grad.data)
print("x2", x2.grad.data)
print("w2", w2.grad.data)
print("b", b.grad.data)
print("x1w1", x1w1.grad.data)
print("x2w2", x2w2.grad.data)
print("x1w1x2w2", x1w1x2w2.grad.data)
print("x1w1x2w2b", x1w1x2w2b.grad.data)
# print("coshx1w1x2w2b", coshx1w1x2w2b.grad.data)
# print("sinhx1w1x2w2b", sinhx1w1x2w2b.grad.data)
print("tanhx1w1x2w2b", tanhx1w1x2w2b.grad.data)
