import CodeBlock from './CodeBlock';
import { Terminal, Book, Code } from 'lucide-react';
import { Session } from '../data/courseContent';

const MarkdownLite = ({ content }: { content: string }) => {
    return (
        <div className="space-y-3 text-slate-300 leading-relaxed font-light">
            {content.split('\n').map((line, i) => {
                const tr = line.trim();
                if (!tr) return <div key={i} className="h-2"></div>;

                if (tr.startsWith('#### ')) {
                    return <h4 key={i} className="text-lg font-semibold text-purple-300 mt-6 mb-2 flex items-center gap-2">
                        <div className="w-1.5 h-1.5 rounded-full bg-purple-400"></div>
                        {tr.replace('#### ', '')}
                    </h4>;
                }
                if (tr.startsWith('### ')) {
                    return <h3 key={i} className="text-xl font-bold text-sky-400 mt-8 mb-4 pb-2 border-b border-slate-700/50 flex items-center gap-2">
                        {tr.replace('### ', '')}
                    </h3>;
                }
                if (tr.startsWith('1. ') || /^\d+\./.test(tr)) {
                    return <div key={i} className="ml-4 pl-4 border-l border-indigo-500/50 py-1 text-slate-300">{tr}</div>;
                }
                if (tr.startsWith('- ') || tr.startsWith('* ')) {
                    return <div key={i} className="ml-6 flex items-start gap-2 relative">
                        <span className="text-indigo-400 mt-1.5 text-[0.6rem]">•</span>
                        <span>{tr.replace(/^[-*] /, '')}</span>
                    </div>;
                }

                // Bold simple parser (**text**)
                const parts = line.split(/(\*\*.*?\*\*)/);
                return (
                    <p key={i}>
                        {parts.map((part, j) => {
                            if (part.startsWith('**') && part.endsWith('**')) {
                                return <strong key={j} className="text-white font-semibold">{part.slice(2, -2)}</strong>;
                            }
                            if (part.startsWith('`') && part.endsWith('`')) {
                                return <code key={j} className="bg-slate-800 px-1.5 py-0.5 rounded text-amber-300 text-sm font-mono border border-white/10">{part.slice(1, -1)}</code>;
                            }
                            return part;
                        })}
                    </p>
                );
            })}
        </div>
    );
};

interface LessonContentProps {
    session: Session;
}

const LessonContent = ({ session }: LessonContentProps) => {
    if (!session) return <div className="p-10 text-center text-slate-500">Selecciona una lección para comenzar</div>;

    return (
        <div className="max-w-4xl mx-auto pb-20 animate-in fade-in duration-500">
            <header className="mb-10">
                <div className="inline-flex items-center gap-2 px-3 py-1 rounded-full bg-blue-500/10 text-blue-400 text-xs font-medium mb-4 border border-blue-500/20">
                    <Book size={12} />
                    <span>Material de Clase</span>
                </div>
                <h2 className="text-4xl font-bold text-white mb-4 tracking-tight">{session.title}</h2>
            </header>

            <div className="glass-panel p-8 mb-8 shadow-2xl shadow-black/20">
                <MarkdownLite content={session.content} />
            </div>

            {session.examples && session.examples.length > 0 && (
                <div className="space-y-8 mt-12">
                    <h3 className="text-2xl font-bold text-white flex items-center gap-3">
                        <Code className="text-green-400" /> Ejemplos Prácticos
                    </h3>
                    {session.examples.map((ex, idx) => (
                        <div key={idx} className="bg-[#0d1117] rounded-xl border border-slate-700/50 shadow-xl overflow-hidden">
                            <div className="bg-white/5 px-4 py-2 border-b border-white/5 text-xs text-slate-400 flex justify-between">
                                <span>ABAP Editor</span>
                                <span>Ejemplo {idx + 1}</span>
                            </div>
                            <CodeBlock code={ex.code} language={ex.language} />
                        </div>
                    ))}
                </div>
            )}

            {session.exercises && session.exercises.length > 0 && (
                <div className="mt-12 p-6 rounded-xl bg-gradient-to-r from-purple-900/20 to-blue-900/20 border border-purple-500/20">
                    <h3 className="text-xl font-bold text-purple-300 mb-4 flex items-center gap-2">
                        <Terminal size={20} />
                        Taller Hands-on
                    </h3>
                    <ul className="space-y-3">
                        {session.exercises.map((ex, idx) => (
                            <li key={idx} className="flex items-start gap-3 text-slate-300">
                                <span className="flex-shrink-0 flex items-center justify-center w-6 h-6 rounded-full bg-purple-500/20 text-purple-300 text-xs border border-purple-500/30">{idx + 1}</span>
                                <span>{ex}</span>
                            </li>
                        ))}
                    </ul>
                </div>
            )}
        </div>
    );
};

export default LessonContent;
