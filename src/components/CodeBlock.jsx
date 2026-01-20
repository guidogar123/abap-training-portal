import React, { useState } from 'react';
import { Prism as SyntaxHighlighter } from 'react-syntax-highlighter';
import { vscDarkPlus } from 'react-syntax-highlighter/dist/esm/styles/prism';
import { Copy, Check } from 'lucide-react';

const CodeBlock = ({ language, code }) => {
    const [copied, setCopied] = useState(false);

    const handleCopy = () => {
        navigator.clipboard.writeText(code);
        setCopied(true);
        setTimeout(() => setCopied(false), 2000);
    };

    return (
        <div className="relative group rounded-lg overflow-hidden border border-[var(--border-color)] my-4">
            <div className="absolute right-2 top-2 z-10 opacity-0 group-hover:opacity-100 transition-opacity">
                <button
                    onClick={handleCopy}
                    className="p-1.5 rounded-md bg-slate-700/80 text-slate-200 hover:bg-slate-600 transition-colors"
                >
                    {copied ? <Check size={16} className="text-green-400" /> : <Copy size={16} />}
                </button>
            </div>
            <SyntaxHighlighter
                language={language || 'abap'}
                style={vscDarkPlus}
                customStyle={{
                    margin: 0,
                    padding: '1.5rem',
                    background: 'var(--code-bg)',
                    fontSize: '0.9rem',
                    lineHeight: '1.5',
                }}
                showLineNumbers={true}
            >
                {code.trim()}
            </SyntaxHighlighter>
        </div>
    );
};

export default CodeBlock;
