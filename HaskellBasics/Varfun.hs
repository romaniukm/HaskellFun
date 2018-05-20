areaCircle r = pi * r ^ 2

halfMinus12  x = x / 2 - 12

areaRect l w = l * w

areaTriangle b h = (b * h) / 2

minus x y = x - y

volBox l w h = l * w * h

volPyramid l w h = l * w * h / 3

areaSquare s = areaRect s s

volCylinder r h = (areaCircle r) * h

heron a b c = sqrt(s * (s - a) * (s - b) * (s - c))
    where
    s = (a + b + c) / 2

areaTriangleTrig a b c = c * height / 2    -- Use trigonometry
    where
    cosa = (b ^ 2 + c ^ 2 - a ^ 2) / (2 * b * c)
    sina = sqrt(1 - cosa ^ 2)
    height = b * sina

areaTriangleHeron a b c = result    -- use Heron's formula
    where
    result = sqrt(s * (s - a) * (s - b) * (s - c))
    s = (a + b + c) / 2
