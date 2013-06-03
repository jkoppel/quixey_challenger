{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleInstances, TemplateHaskell, FlexibleContexts, TemplateHaskell, ImplicitParams #-}

module Kure where

import Prelude hiding (id , (.))

import Control.Category (id, (.))

import Language.Java.Syntax
import Language.KURE (apply, Translate)
import Language.KURE.Utilities (KureM, runKureM)

import KureCong (Context, initialContext)
import Boilerplate

--------------------------------------------------------------------------------

type JMonad = KureM
type JM = JMonad

--------------------------------------------------------------------------------

type TranslateJ a b = Translate Context JMonad a b
type TJ a b = TranslateJ a b
type RewriteJ a = TranslateJ a a
type RJ a = RewriteJ a

applyJ :: TJ a b -> a -> Either String b
applyJ t = runKureM Right Left . apply t initialContext

applyJNoFail :: TJ a b -> a -> b
applyJNoFail t = runKureM id (error "applyJNoFail failed") . apply t initialContext

--------------------------------------------------------------------------------

data GenericJava = GCompilationUnit CompilationUnit
                 | GPackageDecl PackageDecl
                 | GImportDecl ImportDecl
                 | GTypeDecl TypeDecl
                 | GClassDecl ClassDecl
                 | GClassBody ClassBody
                 | GEnumBody EnumBody
                 | GEnumConstant EnumConstant
                 | GInterfaceDecl InterfaceDecl
                 | GInterfaceBody InterfaceBody
                 | GDecl Decl
                 | GMemberDecl MemberDecl
                 | GVarDecl VarDecl
                 | GVarDeclId VarDeclId
                 | GVarInit VarInit
                 | GFormalParam FormalParam
                 | GMethodBody MethodBody
                 | GConstructorBody ConstructorBody
                 | GExplConstrInv ExplConstrInv
                 | GModifier Modifier
                 | GAnnotation Annotation
                 | GElementValue ElementValue
                 | GBlock Block 
                 | GBlockStmt BlockStmt
                 | GStmt Stmt
                 | GCatch Catch
                 | GSwitchBlock SwitchBlock
                 | GSwitchLabel SwitchLabel
                 | GForInit ForInit
                 | GExp Exp
                 | GLiteral Literal
                 | GOp Op
                 | GAssignOp AssignOp
                 | GLhs Lhs
                 | GArrayIndex ArrayIndex
                 | GFieldAccess FieldAccess
                 | GMethodInvocation MethodInvocation
                 | GArrayInit ArrayInit
                 | GType Type
                 | GRefType RefType
                 | GClassType ClassType
                 | GTypeArgument TypeArgument
                 | GWildcardBound WildcardBound
                 | GPrimType PrimType
                 | GTypeParam TypeParam
                 | GIdent Ident
                 | GName Name
   deriving Eq

$(let ?excl = ['BasicFor, 'NormalAnnotation, 'ClassType] in kureYourBoilerplate ''GenericJava)
