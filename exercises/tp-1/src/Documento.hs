module Documento
  ( Doc,
    vacio,
    linea,
    texto,
    foldDoc,
    (<+>),
    indentar,
    mostrar,
    imprimir,
  )
where

data Doc
  = Vacio
  | Texto String Doc
  | Linea Int Doc
  deriving (Eq, Show)

vacio :: Doc
vacio = Vacio

linea :: Doc
linea = Linea 0 Vacio

texto :: String -> Doc
texto t | '\n' `elem` t = error "El texto no debe contener saltos de línea"
texto [] = Vacio
texto t = Texto t Vacio

foldDoc :: (String -> b -> b) -> (Int -> b -> b) -> b -> Doc -> b
foldDoc fTexto fLinea base v = case v of
                              Vacio -> base
                              Texto s doc -> fTexto s (rec doc)
                              Linea n doc -> fLinea n (rec doc)
                              where rec = foldDoc fTexto fLinea base

-- NOTA: Se declara `infixr 6 <+>` para que `d1 <+> d2 <+> d3` sea equivalente a `d1 <+> (d2 <+> d3)`
-- También permite que expresiones como `texto "a" <+> linea <+> texto "c"` sean válidas sin la necesidad de usar paréntesis.
infixr 6 <+>

--Asumiendo que los Doc recibidos como parámetro cumplen el invariante:
--El invariante para los Doc tipo Linea se cumple ya que se no se modifica el valor i que tienen las lines de d1 o d2
--El invariante para los Doc tipo Texto se cumple ya que el texto se modifica concatenando dos strings que ya estaban
--en Docs tipo Texto, entonces no puden ser vacíos o tener saltos de linea. No se pueden crear saltos de linea concatenando
--un string que acaba con \ con uno que empieza con n ya que el primero da error al querer crearlo en forma de Texto
--El único caso durante la recursión donde ambos Doc son de tipo Texto, se concatena sus strings y se usa la componente d
--del segundo, el cuál como cumple con el invariante tiene que ser tipo Vacio o Linea.

(<+>) :: Doc -> Doc -> Doc
d1 <+> d2 = foldDoc (\s acc -> case acc of
            Texto s' doc' -> Texto (s ++ s') doc'
            _ -> Texto s acc) Linea d2 d1

--Asumiendo que el invariante se cumple en los Doc pasados como parámetro, el invariante se cumple en el resultado ya que
--la única modificación que se hace es incrementar el valor que tienen los Doc Linea. Si se trata de restarle valor, tira
--un error, por lo que el valor nunca puede ser menor a 0

-- precondición: i >= 0
indentar :: Int -> Doc -> Doc
indentar i = foldDoc Texto (\n acc -> Linea (i + n) acc) Vacio

mostrar :: Doc -> String
mostrar = foldDoc (++) (\i acc -> "\n" ++ replicate i ' ' ++ acc) ""


-- | Función dada que imprime un documento en pantalla

-- ghci> imprimir (Texto "abc" (Linea 2 (Texto "def" Vacio)))
-- abc
--   def

imprimir :: Doc -> IO ()
imprimir d = putStrLn (mostrar d)
