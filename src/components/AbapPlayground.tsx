import React, { useState } from 'react';
import Editor from '@monaco-editor/react';
import { Transpiler } from '@abaplint/transpiler';
import { ABAP } from '@abaplint/runtime';
import { Play, RotateCcw, Terminal, Code2 } from 'lucide-react';
import { motion, AnimatePresence } from 'framer-motion';

const DEFAULT_CODE = `* ABAP Playground - Prueba tu lógica aquí!
DATA: lv_text TYPE string VALUE 'Hola desde el ABAP Playground!',
      lv_count TYPE i.

WRITE lv_text.

DO 5 TIMES.
  lv_count = sy-index.
  WRITE / |Línea número: \{ lv_count \}|.
ENDDO.

DATA: lt_data TYPE TABLE OF string.
APPEND 'Elemento 1' TO lt_data.
APPEND 'Elemento 2' TO lt_data.

LOOP AT lt_data INTO lv_text.
  WRITE / |Tabla: \{ lv_text \}|.
ENDLOOP.`;

export const AbapPlayground: React.FC = () => {
    const [code, setCode] = useState(DEFAULT_CODE);
    const [output, setOutput] = useState<string[]>([]);
    const [error, setError] = useState<string | null>(null);
    const [isRunning, setIsRunning] = useState(false);

    const runCode = async () => {
        setIsRunning(true);
        setError(null);
        setOutput([]);

        try {
            // @ts-ignore
            const transpiler = new Transpiler();
            // @ts-ignore
            const js = transpiler.run(code);

            // Actual working implementation for abaplint runtime:
            const execute = new Function("runtime", "code", `
                const abap = new runtime.ABAP();
                // Overriding the internal console/write mechanism of abaplint runtime
                let out = "";
                abap.writeln = (t) => { out += (t || "") + "\\n"; };
                abap.write = (t) => { out += (t || ""); };
                
                try {
                    \${js}
                    return out;
                } catch (e) {
                    return "RUNTIME ERROR: " + e.message;
                }
            `);

            const result = execute(ABAP, js);
            setOutput(result.split('\n'));

        } catch (e: any) {
            setError(e.message || "Error desconocido durante la transpilación");
        } finally {
            setIsRunning(false);
        }
    };

    const resetCode = () => {
        setCode(DEFAULT_CODE);
        setOutput([]);
        setError(null);
    };

    return (
        <div className="flex flex-col h-[calc(100vh-120px)] bg-slate-900/50 rounded-2xl border border-white/10 overflow-hidden backdrop-blur-xl">
            {/* Header */}
            <div className="flex items-center justify-between px-6 py-4 border-b border-white/10 bg-white/5">
                <div className="flex items-center gap-3">
                    <div className="p-2 bg-blue-500/20 rounded-lg">
                        <Code2 className="w-5 h-5 text-blue-400" />
                    </div>
                    <div>
                        <h2 className="text-lg font-semibold text-white">ABAP Real-time Playground</h2>
                        <p className="text-xs text-slate-400">Prueba lógica, tablas internas y tipos de datos</p>
                    </div>
                </div>

                <div className="flex gap-2">
                    <button
                        onClick={resetCode}
                        className="flex items-center gap-2 px-4 py-2 text-sm font-medium text-slate-300 hover:text-white hover:bg-white/10 rounded-xl transition-all"
                    >
                        <RotateCcw className="w-4 h-4" />
                        Reiniciar
                    </button>
                    <button
                        onClick={runCode}
                        disabled={isRunning}
                        className={`flex items-center gap-2 px-6 py-2 text-sm font-bold text-white bg-blue-600 hover:bg-blue-500 rounded-xl transition-all shadow-lg shadow-blue-900/20 ${isRunning ? 'opacity-50 cursor-not-allowed' : ''}`}
                    >
                        <Play className="w-4 h-4 fill-current" />
                        {isRunning ? 'Ejecutando...' : 'Ejecutar (F8)'}
                    </button>
                </div>
            </div>

            {/* Main Content */}
            <div className="flex flex-1 overflow-hidden">
                {/* Editor Section */}
                <div className="flex-1 border-r border-white/10 relative">
                    <Editor
                        height="100%"
                        defaultLanguage="abap"
                        theme="vs-dark"
                        value={code}
                        onChange={(value) => setCode(value || "")}
                        options={{
                            minimap: { enabled: false },
                            fontSize: 14,
                            lineNumbers: 'on',
                            roundedSelection: false,
                            scrollBeyondLastLine: false,
                            readOnly: false,
                            automaticLayout: true,
                            padding: { top: 20 }
                        }}
                    />
                </div>

                {/* Output Section */}
                <div className="w-1/3 flex flex-col bg-slate-950/50">
                    <div className="flex items-center gap-2 px-4 py-3 border-b border-white/10 bg-white/5">
                        <Terminal className="w-4 h-4 text-emerald-400" />
                        <span className="text-xs font-bold uppercase tracking-wider text-slate-400">Consola de Salida</span>
                    </div>

                    <div className="flex-1 p-4 font-mono text-sm overflow-y-auto">
                        <AnimatePresence mode="popLayout">
                            {error ? (
                                <motion.div
                                    initial={{ opacity: 0, y: 10 }}
                                    animate={{ opacity: 1, y: 0 }}
                                    className="p-3 rounded-lg bg-red-500/10 border border-red-500/20 text-red-400"
                                >
                                    <span className="font-bold text-xs block mb-1">ERROR DE COMPILACIÓN:</span>
                                    {error}
                                </motion.div>
                            ) : output.length > 0 ? (
                                output.map((line, i) => (
                                    <motion.div
                                        key={i}
                                        initial={{ opacity: 0, x: -10 }}
                                        animate={{ opacity: 1, x: 0 }}
                                        transition={{ delay: i * 0.05 }}
                                        className="text-emerald-400 leading-relaxed min-h-[1.5em]"
                                    >
                                        {line || ' '}
                                    </motion.div>
                                ))
                            ) : (
                                <div className="text-slate-600 italic">
                                    Presiona "Ejecutar" para ver el resultado...
                                </div>
                            )}
                        </AnimatePresence>
                    </div>
                </div>
            </div>

            {/* Footer / Status */}
            <div className="px-4 py-2 bg-black/40 border-t border-white/10 flex justify-between items-center">
                <div className="text-[10px] text-slate-500 flex gap-4">
                    <span>Powered by abaplint</span>
                    <span>Modo: Browser Transpilation</span>
                </div>
                <div className="text-[10px] text-slate-500 italic">
                    Nota: SELECT y SQL no están soportados en este entorno simulado.
                </div>
            </div>
        </div>
    );
};
