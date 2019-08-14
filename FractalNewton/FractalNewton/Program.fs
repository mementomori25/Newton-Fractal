open System
open System.Drawing
open System.Windows.Forms
open System.Numerics


let N = 100
let Deg = 7.0
let Size = 801

let Next number = number - (Complex.Pow(number, Deg) - Complex(1.0, 0.0)) / (Complex(Deg, 0.0) * Complex.Pow(number, (Deg - 1.0)))


let GetRoots deg = 
    let real: float array = Array.zeroCreate deg
    let imag: float array = Array.zeroCreate deg
    real.[0] <- 1.0
    for k in 1..deg - 1 do
        real.[k] <- Math.Cos((2.0 * Math.PI * (float)k) / (float)deg)
        imag.[k] <- Math.Sin((2.0 * Math.PI * (float)k) / (float)deg)
    real, imag // array


let Create x y =
    let spis: Complex array = Array.zeroCreate N
    spis.[0] <- Complex(x, y)
    for i in 1..N - 1 do
        spis.[i] <- Next(spis.[i - 1])
    spis.[N - 1].Real, spis.[N - 1].Imaginary // two most near point


let real, im = GetRoots((int)Deg)

let Norm x1 y1 x2 y2 =
    Math.Sqrt(Math.Pow(x1 - x2, 2.0) + Math.Pow(y1 - y2, 2.0))


let MixInd (A : float array) =
    let mutable min = 1000000.0
    let mutable min_i = 0
    for i in 0..A.Length - 1 do
        if A.[i] < min then
            min <- A.[i]
            min_i <- i
    min_i



let Check x y  =
    let dist: float array = Array.zeroCreate real.Length
    for i in 0..real.Length - 1 do
        let prom = Norm x y real.[i] im.[i]
        dist.[i] <- prom
    MixInd dist



let GetMatrix n = 
    let mutable Matrix2D = [|for i in 0..n - 1 -> [|for j in 0..n - 1 -> [|for k in 0..1 ->  (float)0|]|]|]
    let n_2 = (int)(n - 1)/2
    let mutable i = 0
    let mutable j = 0
    for new_i = n_2 downto (-(n_2)) do
        for new_j in (-n_2)..(n_2) do
            Matrix2D.[i].[j].[0] <- (float)new_j
            Matrix2D.[i].[j].[1] <- (float)new_i
            j <- j + 1
        i <- i + 1
        j <- 0
    Matrix2D




let Result =
    let matrix = GetMatrix(Size)
    let genRandomNumbers count =
        let rnd = System.Random((int) DateTime.Now.Ticks)
        List.init count (fun _ -> rnd.Next (0, 255))
    let RanList = genRandomNumbers ((int)(Deg * 3.0 + 2.0))
    let colors = [for i in 0..(int)Deg - 1 -> Color.FromArgb(255, RanList.[i * 3], RanList.[i * 3 + 1], RanList.[i * 3 + 2])]
    //let image = new Bitmap(Size, Size);
    let form = new Form(Width=Size + 10, Height=Size + 30, StartPosition=FormStartPosition.CenterScreen)
    form.Show()
    let g = form.CreateGraphics()
    let draw color x y = g.FillRectangle(new SolidBrush(color) , new Rectangle(x, y, 1, 1));
    for i in 0..Size-1 do
        for j in 0..Size-1 do
            let x =  matrix.[i].[j].[0]
            let y = matrix.[i].[j].[1]
            let prom = Create x y
            let transform = //trouble
                let x1, x2 = prom
                Check x1 x2
            draw colors.[transform] i j
    Console.ReadKey()