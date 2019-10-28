namespace LWC

open System.Windows.Forms
open System.Drawing

(* Lightweight controls: astrazione programmativa che imita i controlli grafici *)
type LWC() =
  (* Il padre deve essere un Control (sarà un LWContainer) *)
  let mutable parent : Control = null
  let mutable location = PointF()
  let mutable size = SizeF()

  let mutable w2v = new Drawing2D.Matrix()
  let mutable v2w = new Drawing2D.Matrix()

  let mDown = new Event<MouseEventArgs>()
  let mUp = new Event<MouseEventArgs>()
  let mMove = new Event<MouseEventArgs>()
  let mEnter = new Event<MouseEventArgs>()
  let mLeave = new Event<MouseEventArgs>()
  let kDown = new Event<KeyEventArgs>()

  [<CLIEvent>] member this.MouseDown = mDown.Publish
  [<CLIEvent>] member this.MouseUp = mUp.Publish
  [<CLIEvent>] member this.MouseMove = mMove.Publish
  [<CLIEvent>] member this.MouseEnter = mEnter.Publish
  [<CLIEvent>] member this.MouseLeave = mLeave.Publish
  [<CLIEvent>] member this.KeyDown = kDown.Publish

  member this.Invalidate() = if parent <> null then parent.Invalidate()
  member this.Location     with get() = location and set(v) = location <- v; this.Invalidate()
  member this.Size         with get() = size     and set(v) = size <- v; this.Invalidate()
  member this.Parent       with get() = parent   and set(v) = parent <- v
  member this.TransformW2V with get() = w2v      and set(v) = w2v <- v
  member this.TransformV2W with get() = v2w      and set(v) = v2w <- v
  
  abstract OnMouseDown  : MouseEventArgs -> unit; default this.OnMouseDown e  = mDown.Trigger(e)
  abstract OnMouseMove  : MouseEventArgs -> unit; default this.OnMouseMove e  = mMove.Trigger(e)
  abstract OnMouseUp    : MouseEventArgs -> unit; default this.OnMouseUp e    = mUp.Trigger(e)
  abstract OnMouseEnter : MouseEventArgs -> unit; default this.OnMouseEnter e = mEnter.Trigger(e)
  abstract OnMouseLeave : MouseEventArgs -> unit; default this.OnMouseLeave e = mLeave.Trigger(e)
  abstract OnKeyDown    : KeyEventArgs   -> unit; default this.OnKeyDown e    = kDown.Trigger(e)
  abstract OnPaint      : PaintEventArgs -> unit; default this.OnPaint _      = ()
  abstract HitTest      : PointF         -> bool; default this.HitTest p = (RectangleF(PointF(), size)).Contains(p)

type LWContainer() =
  inherit UserControl()

  let controls = ResizeArray<LWC>()
  let mutable captured : LWC option = None

  (* Genera un nuovo evento di tipo MouseEvent *)
  let cloneMouseEvent (c:LWC) (e:MouseEventArgs) =
    let lx, ly = int c.Location.X, int c.Location.Y
    new MouseEventArgs(e.Button, e.Clicks, e.X - lx, e.Y - ly, e.Delta)

  (* La funzione correlate prende in input l'evento 'e' e la funzione 'f' che verrà applicata
     su di 'e', e per ogni controllo grafico 'c', se 'c' è il controllo con il quale l'utente
     ha interagito, allora si applica 'f' al controllo 'c' utilizzando come evento, quello
     generato dalla cloneMouseEvent e non l'evento 'e' passato tra i parametri perchè
     l'handling di questo 'e' è la funzione correlate stessa. *)
  let correlate (e:MouseEventArgs) (f:LWC->MouseEventArgs->unit) =
    let mutable found = false
    for i in { (controls.Count - 1) .. -1 .. 0 } do
      if not found then
        let c = controls.[i]
        if c.HitTest(PointF(single(e.X) - c.Location.X, single(e.Y) - c.Location.Y)) then
          found <- true
          f c (cloneMouseEvent c e)

  member this.LWControls = controls
  member this.Captured with get() = captured
  (* La OnMouseDown chiama la correlate per verificare se il MouseDown è avvenuto all'interno
     di un qualche controllo grafico 'c' - in tal caso setta captured a 'c'. La funzione di
     override termina con la chiamata a base.OnMouseDown. *)
  override this.OnMouseDown e =
    correlate e (fun c ev -> captured <- Some(c); c.OnMouseDown(ev))
    base.OnMouseDown e

  (* La OnMouseMove (come la OnMouseDown) chiama la correlate per verificare se il MouseMove
     è relativo ad un controllo grafico 'c'; se sì, captured sarà settato a Some(c) e quindi
     si gestirà il MouseMove chiamando la OnMouseMove del controllo 'c' su un altro evento,
     clone di 'e'; si termina la funzione di override chiamando la base.OnMouseMove. *)
  override this.OnMouseMove e =
    correlate e (fun c ev -> c.OnMouseMove(ev))
    match captured with
    | Some c -> c.OnMouseMove(cloneMouseEvent c e)
    | None -> ()
    base.OnMouseMove e

  (* La OnMouseUp (come la OnMouseDown) chiama la correlate per verificare se la OnMouseUp è
     relativo ad un controllo grafico 'c'; se sì (captured sarà settato a Some(c)), si gestirà
     il MouseUp chiamando il MouseUp del controllo trovato su un altro evento, clone di 'e'.
     Si termina la funzione di override chiamando la base.OnMouseUp. *)
  override this.OnMouseUp e =
    correlate e (fun c ev -> c.OnMouseUp(ev))
    match captured with
    | Some c -> c.OnMouseUp(cloneMouseEvent c e); captured <- None
    | None -> ()
    base.OnMouseUp e

  (* La OnPaint, esegue una funzione di disegno per ogni controllo presente. La funzione di
     override termina con la chiamata alla base.OnPaint. Nella funzione di disegno:
     1. si salva il contesto grafico, per evitare che modifiche successive lo portino in uno
        stato inconsistente;
     2. si trasla il sistema di riferimento all'inizio del controllo selezionato;
     3. si limita l'area su cui operare all'area relativa al controllo attuale, mediante
        l'operazione di ClipBounds;
     4. si crea l'evento di paint (il PaintEventArgs, che prende in input il rettangolo di
        riferimento del controllo attuale e il contesto grafico);
     5. si chiama l'evento Paint del controllo attuale, passando in input l'evento di paint
        precedentemente creato;
     6. si ripristina il contesto grafico salvato all'inizio. *)
  override this.OnPaint e =
    controls |> Seq.iter (fun c ->
      let s = e.Graphics.Save()
      e.Graphics.TranslateTransform(c.Location.X, c.Location.Y)
      e.Graphics.Clip <- new Region(RectangleF(0.f, 0.f, c.Size.Width, c.Size.Height))
      let r = e.Graphics.ClipBounds
      let evt = new PaintEventArgs(e.Graphics, new Rectangle(int r.Left, int r.Top, int r.Width, int r.Height))
      c.OnPaint evt
      e.Graphics.Restore(s)
    )
    base.OnPaint(e)