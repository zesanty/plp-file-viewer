module PPON where

import Documento

data PPON
  = TextoPP String
  | IntPP Int
  | ObjetoPP [(String, PPON)]
  deriving (Eq, Show)

pponAtomico :: PPON -> Bool
pponAtomico ppon = case ppon of
  ObjetoPP _ -> False
  _ -> True

pponObjetoSimple :: PPON -> Bool
pponObjetoSimple ppon = case ppon of
  ObjetoPP p -> all (pponAtomico . snd) p
  _ -> False
  --   ObjetoPP p -> foldr (&&) True (map (pponAtomico . snd) (p)) -- mucho más texto, usoi all


-- foldr1 define el caso base como foldr1 f [x] = x, si intercalas una lista vacia devolves [],
-- si tiene uno o más elementos usa el caso foldr1
-- def alternativa:
-- intercalar doc docs = foldr (\doc' acc -> doc' <+> doc <+> acc) (last(docs)) (init(docs))
intercalar :: Doc -> [Doc] -> Doc
intercalar _ [] = vacio
intercalar doc xs = foldr1 (\doc' acc -> doc' <+> doc <+> acc) xs

entreLlaves :: [Doc] -> Doc
entreLlaves [] = texto "{ }"
entreLlaves ds =
  texto "{"
    <+> indentar
      2
      ( linea
          <+> intercalar (texto "," <+> linea) ds
      )
    <+> linea
    <+> texto "}"

aplanar :: Doc -> Doc
aplanar =
  foldDoc
    (\s acc -> texto s <+> acc)
    (\_ acc -> texto " " <+> acc)
    vacio

--Es primitiva porque en el caso recursivo de la función (el tercero), p es una lista. La recursión se da en "escribir p" cuando se llama a pponADoc con
--el objeto PPON que está en el primer elemento de la lista. Sin embargo, también usamos la lista entera en otra parte de la función, particularmente la guarda
--del if. Por esta razón la función utiliza recursión primitiva y no estructural
pponADoc :: PPON -> Doc
pponADoc ppon = case ppon of
  TextoPP s -> texto (show s)
  IntPP s -> texto (show s)
  ObjetoPP p -> if pponObjetoSimple (ObjetoPP p) then
              aplanar (entreLlaves (escribir p)) else
              entreLlaves (escribir p)
              where
                escribir = map (\(i,j) -> texto (show i) <+> texto ": " <+> pponADoc j)